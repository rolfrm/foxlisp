#include <dlfcn.h>
#include <iron/full.h>
#include <microio.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "foxlisp.h"
#include "foxlisp_internal.h"
#ifdef WASM
#define VALGRIND_MAKE_MEM_NOACCESS(x, y) ;
#define VALGRIND_MAKE_MEM_DEFINED(x, y) ;
#elif USE_VALGRIND
#include "valgrind/memcheck.h"
#define GCDEBUG
#else
#define VALGRIND_MAKE_MEM_NOACCESS(x, y) ;
#define VALGRIND_MAKE_MEM_DEFINED(x, y) ;
#endif

static void *heap_start = NULL;
static void *heap_end = NULL;

static void *_alloc(size_t v) {
  void *ptr = malloc(v);
  heap_end = MAX(heap_end, ptr + v);
  if (heap_start == NULL)
    heap_start = ptr;
  else
    heap_start = MIN(ptr, heap_start);

  return ptr;
}

static void *_alloc0(size_t v) {
  void *ptr = _alloc(v);
  memset(ptr, 0, v);
  return ptr;
}

static inline bool is_heap_ptr(void *ptr) {
  return ptr >= heap_start && ptr <= heap_end;
}

typedef struct _cons_buffer cons_buffer;

struct _cons_buffer {
  cons_buffer *next;
  cons *free_cons;
  cons *buffer;
  u8 *gc_mark;
  size_t size;
  size_t used;
};

typedef struct __array_header array_header;
struct __array_header {
  // this check is just for verifying the integrety and should always be
  // assigned the value ARRAY_CHECK. Consider removing it in release builds for
  // more performance.
#ifdef GCDEBUG
  size_t reserved;
  size_t check;
#endif
  array_header *next;
  size_t mark;
};

typedef struct __array_pool array_pool;
struct __array_pool {
  array_header *free_arrays;
  array_header *occupied_arrays;
  size_t pool_size;
};

typedef enum {
  CLEAR = 0,
  MARK = 1,
  CLEAR_WEAK = 2,

} gc_stage;

struct __gc_context {
  cons_buffer *cons_pool;
  array_pool array_pool[3 * 1024 + 1];
  gc_stage stage;
  int iteration;
  bool request_gc;
  size_t allocations_counter1;
};

struct __lisp_set {
  lisp_value *values;
  size_t *free_values;
  size_t capacity;
  size_t count;
  size_t free_count;
};

lisp_value lisp_set_new() {
  lisp_set *set = alloc0(sizeof(*set));
  return lisp_set_lisp_value(set);
}
lisp_value lisp_set_lisp_value(lisp_set *set) {
  return (lisp_value){.type = LISP_VALUE_SET, .pointer = set};
}

static size_t _lisp_set_push(lisp_set *set, lisp_value value) {
  if (set->free_count > 0) {
    var idx = set->free_values[0];
    set->free_count -= 1;
    if (set->free_count > 0)
      SWAP(set->free_values[0], set->free_values[set->free_count]);
    set->values[idx] = value;
    return idx;
  }
  if (set->capacity == set->count) {
    size_t new_cap = set->capacity == 0 ? 16 : set->capacity * 2;
    set->values = realloc(set->values, sizeof(set->values[0]) * new_cap);
    set->free_values =
        realloc(set->free_values, sizeof(set->free_values[0]) * new_cap);
    set->capacity = new_cap;
  }

  {
    var idx = set->count;
    set->count += 1;
    set->values[idx] = value;
    return idx;
  }
}

static lisp_value _lisp_set_pop(lisp_set *set, size_t index) {
  if (set->count <= index) {
    raise_string("Index out of bounds for lisp_set");
    return nil;
  }
  lisp_value v = set->values[index];
  set->values[index] = nil;
  set->free_values[set->free_count] = index;
  set->free_count++;
  return v;
}

lisp_value lisp_set_push(lisp_value set, lisp_value obj) {
  lisp_set *setp = lisp_value_pointer(set);
  return integer_lisp_value(_lisp_set_push(setp, obj));
}

lisp_value lisp_set_pop(lisp_value set, lisp_value v) {
  lisp_set *setp = lisp_value_pointer(set);
  return _lisp_set_pop(setp, lisp_value_integer(v));
}

lisp_value lisp_pinned_set = {0};

void *lisp_pin(lisp_value value) {
  var i = lisp_value_integer(lisp_set_push(lisp_pinned_set, value));
  return (void *)(size_t)i;
}

lisp_value lisp_unpin(void *p) {
  var i = (size_t)p;
  return lisp_set_pop(lisp_pinned_set, integer_lisp_value(i));
}

void *lisp_pin_array(lisp_array *values) {

  return lisp_pin(array_lisp_value(values));
}

gc_context *gc_context_new() {
  gc_context *ctx = _alloc0(sizeof(*ctx));
  return ctx;
}

u8 *cons_marker_pointer(gc_context *gc, cons *c) {
  cons_buffer *pool = gc->cons_pool;
  while (pool) {
    size_t offset = c - pool->buffer;
    if (offset < pool->size)
      return pool->gc_mark + offset;
    pool = pool->next;
  }
  return NULL;
}

static inline bool mark_cons(gc_context *gc, cons *c) {
  u8 *marker = cons_marker_pointer(gc, c);
  if (*marker == gc->stage)
    return false;
  *marker = gc->stage;
  return true;
}
static inline bool visit_cons(gc_context *gc, cons *c);
bool gc_mark_cons(gc_context *gc, cons *c) { return visit_cons(gc, c); }

inline bool cons_is_marked(gc_context *gc, cons *c) {
  u8 *marker = cons_marker_pointer(gc, c);
  if (marker == NULL)
    return false;
  return *marker > 0;
}

#define ARRAY_CHECK 0x1122334411223344L

static inline bool mark_vector(gc_context *gc, void *vector) {
  if (vector == NULL)
    return false;
  array_header *mark = vector - sizeof(array_header);
#ifdef GCDEBUG
  ASSERT(mark->check == ARRAY_CHECK);
#endif
  if (mark->mark == gc->stage)
    return false; // already marked.
  mark->mark = gc->stage;
  return true;
}

bool vector_is_marked(void *vector) {
  array_header *mark = vector - sizeof(array_header);
  ASSERT(mark->mark <= 2);
  if (mark->mark)
    return true;
  return false;
}

static inline void visit_value(gc_context *gc, lisp_value val);
void iterate_value(void *key, void *value, void *data);
void iterate_keys_only(void *key, void *value, void *data);
void iterate_values_only(void *key, void *value, void *data);
void iterate_scope(gc_context *ctx, lisp_scope *scope, bool check_mark);

static ht_op iterate_keys_remove_unmarked(void *key, void *value, void *data) {
  lisp_value *ke = key;
  lisp_value *val = value;
  gc_context *gc = data;
  switch (ke->type) {
  case LISP_CONS:
    if (!cons_is_marked(gc, ke->cons))
      return HT_REMOVE;
    break;
  default:
    break;
  }
  visit_value(gc, *val);

  return HT_NOP;
}

static ht_op iterate_values_remove_unmarked(void *key, void *value,
                                            void *data) {
  UNUSED(key);
  lisp_value *val = value;
  gc_context *gc = data;
  switch (val->type) {
  case LISP_CONS:
    if (!cons_is_marked(gc, val->cons))
      return HT_REMOVE;
    break;
  default:
    break;
  }
  return HT_NOP;
}

static inline void mark_scope(gc_context *gc, lisp_scope *scope) {
  if (scope == NULL)
    return;
  bool is_heap = is_heap_ptr(scope);
  if (is_heap && !mark_vector(gc, scope))
    return;
  else if (!is_heap) {
    if (scope->non_heap_mark != gc->iteration)
      scope->non_heap_mark = gc->iteration;
    else
      return;
  }
  iterate_scope(gc, scope, false);
}

static inline bool visit_cons(gc_context *gc, cons *c) {
  if (is_heap_ptr(c) && !mark_cons(gc, c))
    return false;
  visit_value(gc, c->car);
  visit_value(gc, c->cdr);
  return true;
}

static inline void visit_hashtable(gc_context *gc, hash_table *table) {
  if (!(mark_vector(gc, table) | mark_vector(gc, table->keys) |
        mark_vector(gc, table->elems) | mark_vector(gc, table->occupied)))
    return;

  // weak key / value table support
  if (table->userdata != NULL) {
    size_t mask = (size_t)table->userdata;
    if (mask & LISP_HASHTABLE_WEAK_KEYS) {
      if (gc->stage == CLEAR_WEAK) {
        // removed non-gc-marked entries
        ht_iterate2(table, iterate_keys_remove_unmarked, gc);
      } else {
        ht_iterate(table, iterate_values_only, gc);
      }
    } else if (mask & LISP_HASHTABLE_WEAK_VALUES) {
      // note unmarked.
      ht_iterate(table, iterate_values_only, gc);
      if (gc->stage == CLEAR_WEAK) {
        // removed non-gc-marked entries
        ht_iterate2(table, iterate_values_remove_unmarked, gc);
      }
    }
  } else {
    ht_iterate(table, iterate_value, gc);
  }
}

static inline void visit_value(gc_context *gc, lisp_value val) {

  switch (val.type) {
  case LISP_CONS:
    visit_cons(gc, val.cons);
    return;
  case LISP_FUNCTION_MACRO:
  case LISP_FUNCTION:
    if (is_heap_ptr(val.function) && !mark_vector(gc, val.function))
      return;
    iterate_scope(gc, val.function->closure, true);
    visit_value(gc, val.function->code);
    visit_value(gc, val.function->args);
    return;
  case LISP_FUNCTION_NATIVE:
    if (is_heap_ptr(val.nfunction) && !mark_vector(gc, val.nfunction))
      return;
    return;
  case LISP_HASHTABLE:
    visit_hashtable(gc, val.native_pointer);
    break;
  case LISP_ALLOCATED_POINTER:
    if (!mark_vector(gc, val.native_pointer))
      return;
    break;
  case LISP_NATIVE_POINTER_TO_VALUE:
    // assuming a non-gc'd pointer
    {
      lisp_value *ptr_to_value = val.native_pointer;
      if (ptr_to_value != NULL)
        visit_value(gc, *ptr_to_value);
    }
    break;
  case LISP_SCOPE:
    mark_scope(gc, val.scope);
    break;
  case LISP_NATIVE_VECTOR:
  case LISP_VECTOR:
  
    if (!mark_vector(gc, val.vector))
      return;
    if (val.type != LISP_NATIVE_VECTOR && !mark_vector(gc, val.vector->data))
      return;
    switch (val.vector->default_value.type) {
    case LISP_INTEGER:
    case LISP_INTEGER32:
    case LISP_RATIONAL:
    case LISP_SYMBOL:
    case LISP_BYTE:
    case LISP_FLOAT32:
    case LISP_GLOBAL_INDEX:
    case LISP_LOCAL_INDEX:
    case LISP_GLOBAL_CONS_ARRAYS: // maybe an error
    case LISP_VALUE_SET:          // maybe an error?
    case LISP_ARRAY:
    case LISP_TYPESPEC:

      return;
    case LISP_NIL:
    case LISP_T:
    case LISP_CONS:
    case LISP_FUNCTION:
    case LISP_FUNCTION_MACRO:
    case LISP_FUNCTION_NATIVE:
    case LISP_STRING:
    case LISP_MACRO_BUILTIN:
    case LISP_NATIVE_VECTOR:
    case LISP_VECTOR:
    case LISP_NATIVE_POINTER:
    case LISP_SCOPE:
    case LISP_ALLOCATED_POINTER:
    case LISP_NATIVE_POINTER_TO_VALUE:
    case LISP_HASHTABLE:
      // elem type is lisp_value
      {
        lisp_value *values = val.vector->data;
        size_t count = val.vector->count;
        for (size_t i = 0; i < count; i++) {
          visit_value(gc, values[i]);
        }
      }
      return;
    }
    break;
  case LISP_STRING:
    if (is_heap_ptr(val.string))
      mark_vector(gc, val.string);
    break;

  case LISP_NATIVE_POINTER:
  case LISP_MACRO_BUILTIN:
  case LISP_INTEGER:
  case LISP_INTEGER32:
  case LISP_NIL:
  case LISP_T:
  case LISP_RATIONAL:
  case LISP_SYMBOL:
  case LISP_BYTE:
  case LISP_FLOAT32:
  case LISP_GLOBAL_INDEX:
  case LISP_LOCAL_INDEX:
    return;
  case LISP_GLOBAL_CONS_ARRAYS: {
    t_cons_arrays *ca = lisp_value_pointer(val);
    for (size_t i = 0; i < ca->count; i++) {
      t_cons_array *ar = ca->arrays + i;
      for (size_t j = 0; j < ar->count; j++) {
        visit_value(gc, ar->array[j].car);
        visit_value(gc, ar->array[j].cdr);
      }
    }
    break;
  }
  case LISP_VALUE_SET: {
    lisp_set *set = lisp_value_pointer(val);
    for (size_t i = 0; i < set->count; i++) {
      var sub_value = set->values[i];
      if (!is_nil(sub_value))
        visit_value(gc, sub_value);
    }
  } break;
  case LISP_ARRAY: {

    lisp_array *array = lisp_value_pointer(val);
    for (size_t i = 0; i < array->count; i++) {
      var sub_value = array->array[i];
      if (!is_nil(sub_value))
        visit_value(gc, sub_value);
    }

  } break;
  case LISP_TYPESPEC: {
    var typespec = val.typespec;
    if (mark_vector(gc, typespec)) {
      visit_value(gc, typespec->print);
      visit_value(gc, typespec->destruct);
      visit_value(gc, typespec->generic_lookup);
      visit_value(gc, typespec->name);
      visit_value(gc, typespec->construct);
    }
  }
  }
}

void iterate_value(void *key, void *value, void *data) {
  lisp_value *val = value;
  lisp_value *ke = key;
  gc_context *gc = data;
  visit_value(gc, *val);
  visit_value(gc, *ke);
}

void iterate_values_only(void *key, void *value, void *data) {
  UNUSED(key);
  lisp_value *val = value;
  gc_context *gc = data;
  visit_value(gc, *val);
}

void iterate_keys_only(void *key, void *value, void *data) {
  UNUSED(value);
  lisp_value *ke = key;
  gc_context *gc = data;
  visit_value(gc, *ke);
}

void gc_clear(gc_context *gc) {
  gc->iteration += 1;
  var buf = gc->cons_pool;

  while (buf != NULL) {
    memset(buf->gc_mark, 0, buf->size * sizeof(buf->gc_mark[0]));

    buf = buf->next;
  }

  array_pool *array_pools = gc->array_pool;
  for (size_t i = 0; i < 3 * 1024 + 1; i++) {
    var ptr = array_pools[i].occupied_arrays;
    while (ptr) {

      var header = ptr;
#ifdef GCDEBUG
      ASSERT(header->check == ARRAY_CHECK);
#endif
      header->mark = 0;
      ptr = header->next;
    }
  }
}

static inline void recover_cons(cons_buffer *buf, ssize_t i) {
  if (buf->gc_mark[i] == false) {
    buf->buffer[i].cdr.cons = buf->free_cons;
    buf->buffer[i].car = nil;
    buf->free_cons = buf->buffer + i;
    buf->used -= 1;
  }
}

static void recover_pool(cons_buffer *buf) {
  // free them in inverse order
  // this means
  // always a multiple of 8 bytes.
  u64 *marks = (u64 *)buf->gc_mark;
  size_t mark_count = buf->size /*used*/ / 8;
  for (ssize_t j = mark_count; j > 0; j--) {
    // 02020202... - the GC mark when a full block is all marked
    // 2 is the stage 2 GC mark.
    if (marks[j - 1] == 0x0202020202020202L)
      continue;

    ssize_t i2 = j * 8;
    for (ssize_t i = 0; i < 8; i += 1) {
      recover_cons(buf, i2 - i - 1);
    }
  }
}
static inline void cons_call_destructors(cons_buffer *buf, ssize_t i) {
  if (buf->gc_mark[i] == false) {
    cons *c = &buf->buffer[i];
    if (c->car.type == LISP_TYPESPEC) {
      var destruct = c->car.typespec->destruct;
      if (!is_nil(destruct)) {
        var c2 = cons_lisp_value(c);
        lisp_eval_quoted2(current_context->globals, destruct, c2);
      }
    }
  }
}

static void gc_call_destructors(cons_buffer *buf) {
  while (buf != NULL) {

    u64 *marks = (u64 *)buf->gc_mark;
    size_t mark_count = buf->size / 8;
    for (size_t j = 0; j < mark_count; j++) {
      // 02020202... - the GC mark when a full block is all marked
      // 2 is the stage 2 GC mark.
      if (marks[j] == 0x0202020202020202L)
        continue;

      size_t i2 = j * 8;
      for (size_t i = 0; i < 8; i += 1) {
        cons_call_destructors(buf, i2 + i);
      }
    }
    buf = buf->next;
  }
}

void gc_recover_unmarked(gc_context *gc) {
  // call destructors

  gc_call_destructors(gc->cons_pool);
  // recover cons.
  var buf = gc->cons_pool;
  while (buf != NULL) {
    cons *c = buf->free_cons;
    while (c != NULL) {
      // first mark all the already free conses.
      mark_cons(gc, c);
      c = c->cdr.cons;
    }
    recover_pool(buf);
    buf = buf->next;
  }

  array_pool *array_pools = gc->array_pool;
  for (size_t i = 0; i < 3 * 1024 + 1; i++) {
    var pool = array_pools + i;
    array_header *free = pool->occupied_arrays;
    while (free) {
      // ASSERT(free->check == ARRAY_CHECK);
      free = free->next;
    }

    array_header **place = &pool->occupied_arrays;

    while (*place) {
      array_header *obj = *place;
#ifdef GCDEBUG
      ASSERT(obj->check == ARRAY_CHECK);
#endif
      if (obj->mark == 0) {
        // recover it this array by popping it from the list.
        array_header *next = obj->next;
        obj->next = pool->free_arrays;
        pool->free_arrays = obj;
        ASSERT(pool->pool_size > 0);
        VALGRIND_MAKE_MEM_NOACCESS((void *)(obj + 1), pool->pool_size);

        *place = next;

      } else if (obj->mark <= 2) {
        place = &obj->next;
      } else {
        ASSERT(false);
      }
    }
  }
}

void iterate_scope(gc_context *ctx, lisp_scope *scope, bool check_mark) {
  if (scope->stack_scope == false && check_mark && !mark_vector(ctx, scope)) {
    return;
  }

  if (scope->values != NULL) {
    for (size_t i = 0; i < scope->values_count; i++) {
      visit_value(ctx, scope->values[i]);
    }
  }
  var cns = scope->lookup;
  if (!scope->lookup_on_stack && scope->lookup != NULL)
    mark_vector(ctx, scope->lookup);

  for (size_t i = 0; i < scope->argcnt; i++) {
    visit_value(ctx, cns[i].cdr);
    visit_value(ctx, cns[i].car);
  }

  if (scope->sub_scope != NULL)
    mark_scope(ctx, scope->sub_scope);

  if (scope->super == NULL)
    return;

  if (is_heap_ptr(scope->super)) {
    if (!mark_vector(ctx, scope->super))
      return;
  }
  iterate_scope(ctx, scope->super, false);
}

int gc_unsorted_cons(gc_context *gc) {
  int cnt = 0;
  var buf = gc->cons_pool;

  while (buf != NULL) {
    cons *c = buf->free_cons;
    while (c) {
      if (c > c->cdr.cons) {
        cnt += 1;
      }
      c = c->cdr.cons;
    }
    buf = buf->next;
  }
  return cnt;
}

size_t ht_count(hash_table *ht);

void gc_mark(lisp_context *lisp) {
  lisp->gc->stage = MARK;
  iterate_scope(lisp->gc, lisp->globals, true);
}

// todo: check if there is a performance benefit to adding gc weak objects.

void gc_clear_weak(lisp_context *lisp) {
  lisp->gc->stage = CLEAR_WEAK;
  iterate_scope(lisp->gc, lisp->globals, true);
}

void gc_collect_garbage(lisp_context *lisp) {
  
  var gc = lisp->gc;
  gc_clear(gc);
  gc_mark(lisp);
  // to support weak references I had to add a stage to the garbage collection.
  // A second sweep is done and every weak reference is cleared
  // if the object being pointed to is unmarked at this stage.
  // Currently only a limited set of objects can be weak.
  // - add support for an object where a callback is done when the target
  // object is removed. This could be useful for resources that needs to be
  // managed.

  gc_clear_weak(lisp);
  gc_recover_unmarked(gc);
}

bool gc_unsafe_stack = false;
void maybe_gc(lisp_context * ctx){
  if(gc_unsafe_stack == false && ctx->gc->request_gc){
    gc_collect_garbage(ctx);
    ctx->gc->request_gc = false;
  }
}

lisp_value new_cons(lisp_value _car, lisp_value _cdr) {
  // bool gc_run = false;
  // start:
  var ctx = current_context->gc;
  ctx->allocations_counter1 += 1;
  while (true) {
    cons_buffer *pool = ctx->cons_pool;
    while (pool != NULL) {
      if (pool->free_cons != NULL) {

        var found = pool->free_cons;
        pool->free_cons = found->cdr.cons;
        found->car = _car;
        found->cdr = _cdr;
        pool->used += 1;
        if(pool->next == NULL && (pool->size - pool->used) < 32 && ctx->allocations_counter1 > 32 ){
          // if this happens, we should start thinking about collecting garbage.
          // also allocate if it is more than 32 allocations since the last one.
          ctx->request_gc = true;
          ctx->allocations_counter1 = 0;
        }
        
        return cons_lisp_value(found);
      } else {
        pool = pool->next;
      }
    }

    size_t pool_size = 256;
    if (pool == NULL) {
      cons_buffer **parent = &ctx->cons_pool;
      while (*parent != NULL) {
        pool_size *= 4;
        parent = &(*parent)->next;
      }
      if (pool_size > 1024 * 64) {
        printf("Pool size %i\n", (int)pool_size);
        raise(SIGINT);
      }

      cons_buffer *new_pool = _alloc0(sizeof(*pool));
      new_pool->buffer = _alloc0(sizeof(cons) * pool_size);
      new_pool->gc_mark = _alloc0(sizeof(bool) * pool_size);
      new_pool->size = pool_size;

      new_pool->free_cons = new_pool->buffer;
      for (size_t i = 0; i < pool_size - 1; i++) {
        new_pool->buffer[i].cdr.cons = &new_pool->buffer[i + 1];
        new_pool->buffer[i].car = nil;
      }
      *parent = new_pool;
    }
  }
  // this should never happen.
  return nil;
}

void *gc_clone(const void *mem, size_t s) {

  void *d = lisp_malloc(s);
  memcpy(d, mem, s);
  return d;
}

void *nogc_clone(const void *mem, size_t s) {

  void *d = _alloc(s);
  memcpy(d, mem, s);
  return d;
}

size_t allocated = 0;
bool trace_allocations = false;

lisp_value lisp_trace_allocations(lisp_value c) {
  trace_allocations = !is_nil(c);
  return nil;
}

typedef struct {
  size_t alloc_size;
  size_t pool_id;
} pool_info;

static inline pool_info get_array_pool(size_t v) {
  // only allocate multiples of 8 byte to promote reuse.
  v = v + 1;
  v = 1 + (v - 1) / 8;
  v = v * 8;

  size_t poolid = 0;
  if (v < 1024) {
    // small_pool
    poolid = v;
  } else if (v < 1024 * 1024) {
    // 1k-1M size pool
    poolid = v / 1024 + 1024;
    v = (1 + v / 1024) * 1024;
  } else if (v < 1024 * 1024 * 1024) {
    // <1M sized pools.
    poolid = v / (1024 * 1024) + 1024 + 1024;
    v = (1 + v / (1024 * 1024)) * 1024 * 1024;
  } else {
    // > 1M
    poolid = 1024 + 1024 + 1024;
  }
  return (pool_info){.alloc_size = v, .pool_id = poolid};
}

// always allocates cleared pointers.
static inline void *pool_alloc_array(gc_context *ctx, pool_info p) {
  array_header *new_ptr = ctx->array_pool[p.pool_id].free_arrays;
  if (new_ptr) {
    ctx->array_pool[p.pool_id].free_arrays = new_ptr->next;
    new_ptr->next = ctx->array_pool[p.pool_id].occupied_arrays;
    ctx->array_pool[p.pool_id].occupied_arrays = new_ptr;
#ifdef GCDEBUG
    ASSERT(new_ptr->check == ARRAY_CHECK);
#endif
    void *r = new_ptr + 1;
    ASSERT((r - sizeof(array_header)) == new_ptr);
    VALGRIND_MAKE_MEM_DEFINED(r, p.alloc_size);
    memset(r, 0, p.alloc_size);
    return r;
  }
  array_header *arr = _alloc0(p.alloc_size + sizeof(array_header) + 8);
#ifdef GCDEBUG
  arr->check = ARRAY_CHECK;
#endif
  arr->next = ctx->array_pool[p.pool_id].occupied_arrays;
  VALGRIND_MAKE_MEM_NOACCESS(&arr->reserved, sizeof(arr->reserved));
  ctx->array_pool[p.pool_id].occupied_arrays = arr;
  allocated += p.alloc_size;
  ctx->array_pool[p.pool_id].pool_size = p.alloc_size;

  void *outptr = arr + 1;
  VALGRIND_MAKE_MEM_NOACCESS(outptr + p.alloc_size, 8);

  return outptr;
}

inline void *lisp_malloc(size_t v) {
  // cannot default to a normal allocator because that has the potential of
  // messing with the garbage collector later on.
  ASSERT(current_context != NULL);
  if (v == 0)
    return NULL;
  var pool_info = get_array_pool(v);
  return pool_alloc_array(current_context->gc, pool_info);
}

void *lisp_realloc(void *p, size_t v) { return realloc(p, v); }

lisp_value lisp_get_allocated() { return integer(allocated); }

void lisp_free(void *p) {
  // this function is not really implemented
  // it is not currently allowed to 'help'
  // the garbage collector by explicitly freeing memory.
  UNUSED(p);
  // free(p);
}

lisp_value lisp_count_allocated() {
  var gc = current_context->gc;
  size_t allocated = 0;
  var buf = gc->cons_pool;

  while (buf != NULL) {
    allocated += buf->size;
    var free = buf->free_cons;
    while (free) {
      allocated -= 1;
      free = free->cdr.cons;
    }
    buf = buf->next;
  }

  array_pool *array_pools = gc->array_pool;
  for (size_t i = 0; i < 3 * 1024 + 1; i++) {
    var ptr = array_pools[i].occupied_arrays;
    while (ptr) {
      allocated += i;
      array_header *next = ptr;
      ptr = next->next;
    }
  }
  return integer(allocated);
}

void gc_register() {
  lisp_pinned_set = lisp_set_new();
  lisp_register_value("gc:++pinned-set++",
                      lisp_pointer_to_lisp_value(&lisp_pinned_set));
}
