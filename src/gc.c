#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>
#include <dlfcn.h>

#include "foxlisp.h"

static void * heap_start = NULL;
static void * heap_end = NULL;

static void * _alloc(size_t v){
  void * ptr = malloc(v);
  heap_end = MAX(heap_end, ptr + v);
  if(heap_start == NULL)
    heap_start = ptr;
  else
    heap_start = MIN(ptr, heap_start);
  
  return ptr;    
}


static void * _alloc0(size_t v){
  void * ptr = _alloc(v);
  memset(ptr, 0, v);
  return ptr;    
}


bool is_heap_ptr(void * ptr){
  return ptr >= heap_start && ptr <= heap_end;
}

typedef struct _cons_buffer cons_buffer;

struct _cons_buffer {
  cons_buffer * next;
  cons * free_cons;
  cons * buffer;
  bool * gc_mark;
  size_t size;
};

typedef struct __array_header array_header; 
struct __array_header{
  // this check is just for verifying the integrety and should always be assigned
  // the value 0x123.
  // Consider removing it in release builds for more performance.
  size_t check;
  array_header * next;
  size_t mark;
  
};

typedef struct __array_pool array_pool;
struct __array_pool{
  array_header * free_arrays;
  array_header * occupied_arrays; 
};


struct __gc_context{
  cons_buffer * cons_pool;
  array_pool array_pool[3 * 1024 + 1];
};

gc_context * gc_context_new(){
  gc_context * ctx = _alloc0(sizeof(*ctx));
  return ctx;
}

inline bool * cons_marker_pointer(gc_context * gc, cons * c){
  cons_buffer * pool = gc->cons_pool;
  while(pool){
    size_t offset = c - pool->buffer;
    if(offset < pool->size)
      return pool->gc_mark + offset;
    pool = pool->next;
  }
  return NULL;
}

static bool mark_cons(gc_context * gc, cons * c){
  bool * marker = cons_marker_pointer(gc, c);
  if(marker == NULL){
    ASSERT(false);
    return false;
  }
  if(*marker)
    return false;
  *marker = true;
  return true;
}
static inline bool visit_cons(gc_context * gc, cons * c);
bool gc_mark_cons(gc_context * gc, cons * c){
  return visit_cons(gc, c);
}

bool cons_is_marked(gc_context * gc, cons * c){
  bool * marker = cons_marker_pointer(gc, c);
  if(marker == NULL)
    return false;
  return *marker;
}

static inline bool mark_vector(gc_context * gc, void * vector){
  if(vector == NULL) return false;
  array_header * mark = vector - sizeof(array_header);
  ASSERT(mark->check == 0x123);
  ASSERT(mark->mark <= 1);
  if(mark->mark)
    return false; // already marked.
  mark->mark = 1;
  return true;
}


bool vector_is_marked(gc_context * gc, void * vector){
  array_header * mark = vector - sizeof(array_header);
  ASSERT(mark->mark <= 1);
  if(mark->mark)
    return true;
  return false;
}


static inline void visit_value(gc_context * gc, lisp_value val);
void iterate_value(void * key, void * value, void * data);
void iterate_scope(gc_context * ctx, lisp_scope * scope);
void mark_scope(gc_context * gc, lisp_scope * scope){
  if(scope == NULL)
    return;
  if(is_heap_ptr(scope) && !mark_vector(gc, scope))
    return;
  iterate_scope(gc, scope);
}

static inline bool visit_cons(gc_context * gc, cons * c){
  if(is_heap_ptr(c) && !mark_cons(gc, c)) 
    return false;
  visit_value(gc, c->car);
  visit_value(gc, c->cdr);
  return true;
}
static inline void visit_value(gc_context * gc, lisp_value val){
  
  switch(val.type){
  case LISP_CONS:
    visit_cons(gc, val.cons);
    return;  
  case LISP_FUNCTION_MACRO:
  case LISP_FUNCTION:
    if(is_heap_ptr(val.function) && !mark_vector(gc, val.function))
      return;
    iterate_scope(gc, val.function->closure);
    visit_value(gc, val.function->code);
    visit_value(gc, val.function->args);
    return;
  case LISP_FUNCTION_NATIVE:
    if(is_heap_ptr(val.nfunction) && !mark_vector(gc, val.nfunction))
      return;
    return;
  case LISP_HASHTABLE:
    {
      hash_table * table = val.native_pointer;
      if(!mark_vector(gc, table) && !mark_vector(gc, table->keys)) return;
      ht_iterate(table, iterate_value, gc);
      mark_vector(gc, table->elems);
      mark_vector(gc, table->occupied);
    }
    return;
  case LISP_ALLOCATED_POINTER:
    if(!mark_vector(gc, val.native_pointer))
      return;
  case LISP_NATIVE_POINTER_TO_VALUE:
    // assuming a non-gc'd pointer
    lisp_value * ptr_to_value = val.native_pointer;
    if(ptr_to_value != NULL)
      visit_value(gc, *ptr_to_value);
    break;
  case LISP_SCOPE:
    mark_scope(gc, val.scope);
    break;
    
  case LISP_VECTOR:
    if(!mark_vector(gc, val.vector))
      return;
    if(!mark_vector(gc, val.vector->data))
      return;
    switch (val.vector->default_value.type){
    case LISP_INTEGER:
    case LISP_INTEGER32:
    case LISP_RATIONAL:
    case LISP_SYMBOL:
    case LISP_BYTE:
    case LISP_FLOAT32:
    case LISP_GLOBAL_INDEX:
    case LISP_LOCAL_INDEX:
      return;
    case LISP_NIL:
    case LISP_T:
    case LISP_CONS:
    case LISP_FUNCTION:
    case LISP_FUNCTION_MACRO:
    case LISP_FUNCTION_NATIVE:
    case LISP_STRING:
    case LISP_MACRO_BUILTIN:
    case LISP_VECTOR:
    case LISP_NATIVE_POINTER:
    case LISP_SCOPE:
    case LISP_ALLOCATED_POINTER:
    case LISP_NATIVE_POINTER_TO_VALUE:
    case LISP_HASHTABLE:
      // elem type is lisp_value
      {
        lisp_value * values = val.vector->data;
        size_t count = val.vector->count;
        for(size_t i = 0; i < count; i++){
          visit_value(gc, values[i]);
        }
      }
      return;
    }
  case LISP_STRING:
    if(is_heap_ptr(val.string))
      mark_vector(gc, val.string);
    return;
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
  }
}

void iterate_value(void * key, void * value, void * data){
  lisp_value * val = value;
  lisp_value * ke = key;
  gc_context * gc = data;
  visit_value(gc, *val);
  visit_value(gc, *ke);
}

void gc_clear(gc_context * gc){
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    memset(buf->gc_mark, 0, buf->size * sizeof(buf->gc_mark[0]));

    buf = buf->next;
  }
  
  array_pool * array_pools = gc->array_pool;
  for(size_t i = 0; i < 3 * 1024 + 1; i++){
    var ptr = array_pools[i].occupied_arrays;
    while(ptr){
      var header = ptr;
      header->mark = 0;
      ptr = header->next;
    }
  }
}

void gc_recover_unmarked(gc_context * gc){
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    cons * c = buf->free_cons;
    while(c != NULL){
      // first mark all the already free conses.
      mark_cons(gc, c);
      c = c->cdr.cons;
    }
    // free them in inverse order
    // this means
    for(size_t i = buf->size - 1; i >= 0; i--){
      if(buf->gc_mark[i] == false){
        buf->buffer[i].cdr.cons = buf->free_cons;
        buf->buffer[i].car = nil;
        //ASSERT(is_heap_ptr(buf->buffer + i));
        buf->free_cons = buf->buffer + i;
      }
      if(i == 0)break;
    }
    buf = buf->next;
  }
  //return;
  array_pool * array_pools = gc->array_pool;
  for(size_t i = 0; i < 3 * 1024 + 1; i++){
    var pool = array_pools + i;
    array_header ** place = &array_pools[i].occupied_arrays;
    while(*place){
      array_header * obj = *place;
      
      if(obj->mark == 0){
        //recover it this array by popping it from the list.
        array_header * next = obj->next;
        obj->next = pool->free_arrays;
        pool->free_arrays = obj;
        *place = next;

      }else if(obj->mark == 1){
        place = &obj->next;
      }else{
        ASSERT(false);
      }
    }
  }
}

void iterate_scope(gc_context * ctx, lisp_scope * scope){
  if(scope->stack_scope == false && !mark_vector(ctx, scope))
    return;
    
  if(scope->values != NULL){
    for(int i = 0; i < scope->values_count; i++){
      visit_value(ctx, scope->values[i]);
    }
  }
  var cns = scope->lookup;
  if(!scope->lookup_on_stack && scope->lookup != NULL)
    mark_vector(ctx, scope->lookup);

  for(size_t i = 0; i < scope->argcnt; i++){
    visit_value(ctx, cns[i].cdr);
    visit_value(ctx, cns[i].car);
  }
  
  if(scope->sub_scope != NULL)
    mark_scope(ctx, scope->sub_scope);
  
  if(scope->super == NULL) return;
  
  if(is_heap_ptr(scope->super)){
    if(!mark_vector(ctx, scope->super))
      return;
  }
  iterate_scope(ctx, scope->super);
}

int gc_unsorted_cons(gc_context * gc){
  int cnt = 0;
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    cons * c = buf->free_cons;
    while(c){
      if(c > c->cdr.cons){
        cnt +=1;
      }
      c = c->cdr.cons;
    }
    buf = buf->next;
  }
  return cnt;
}

size_t ht_count(hash_table * ht);

void gc_mark(lisp_context * lisp){
  iterate_scope(lisp->gc, lisp->globals);
}

void gc_collect_garbage(lisp_context * lisp){
  var gc = lisp->gc;
  gc_clear(gc);
  gc_mark(lisp);
  gc_recover_unmarked(gc);
}

extern lisp_context * current_context;

lisp_value new_cons(lisp_value _car, lisp_value _cdr){
  var ctx = current_context->gc;
   while(true){
    var pool = ctx->cons_pool;
    while(pool != NULL){
      if(pool->free_cons != NULL){
        
        var found = pool->free_cons;
        pool->free_cons = found->cdr.cons;
        found->car = _car;
        found->cdr = _cdr;
        return (lisp_value){.type = LISP_CONS, .cons = found};
      }else{
        pool = pool->next;
      }
    }
    size_t pool_size = 1024 * 8;
    if(pool == NULL){
      cons_buffer ** parent = &ctx->cons_pool;
      while(*parent != NULL){
        pool_size *= 4;
        parent = &(*parent)->next;
      }
      printf("new pool %i\n", pool_size);
      
      cons_buffer * new_pool = _alloc0(sizeof(*pool));
      new_pool->buffer = _alloc0(sizeof(cons) * pool_size);
      new_pool->gc_mark = _alloc0(sizeof(bool) * pool_size);
      new_pool->size = pool_size;
      
      new_pool->free_cons = new_pool->buffer;
      for(size_t i = 0; i < pool_size - 1; i++){
        new_pool->buffer[i].cdr.cons = &new_pool->buffer[i + 1];
        new_pool->buffer[i].car = nil;                 
      }
      *parent = new_pool;
    }
  }
  // this should never happen.
  return nil;
}

void * gc_clone(const void * mem, size_t s){

  void * d = lisp_malloc(s);
  memcpy(d, mem, s);
  return d;
}

void * nogc_clone(const void * mem, size_t s){
  
  void * d = _alloc(s);
  memcpy(d, mem, s);
  return d;
}


size_t allocated = 0;
bool trace_allocations = false;

lisp_value lisp_trace_allocations(lisp_value c){
  trace_allocations = !is_nil(c);
  return nil;
}
void * lisp_malloc(size_t v){

  // cannot default to a normal allocator because that has the potential of messing
  // with the garbage collector later on.
  //if(current_context == NULL)return _alloc(v);
  ASSERT(current_context != NULL);
  var ctx = current_context->gc;
  
  size_t poolid = 0;
  if(v < 1024){
    // small_pool
    poolid = v;
    //v += 1;
  }else if(v < 1024 * 1024){
      poolid = v / 1024 + 1024;
      //v += 1024;
  }else if(v < 1024 * 1024 * 1024){
    poolid = v / (1024 * 1024) + 1024 + 1024;
    //v += 1024 * 1024;
  }else {
    poolid = 1024 + 1024 + 1024;
  }

  if(ctx->array_pool[poolid].free_arrays){
    array_header *  new_ptr = ctx->array_pool[poolid].free_arrays;
    
    ctx->array_pool[poolid].free_arrays = new_ptr->next;
    new_ptr->next = ctx->array_pool[poolid].occupied_arrays;
    ctx->array_pool[poolid].occupied_arrays = new_ptr;
    void * r = new_ptr + 1; 
    memset(r, 0, v);
    return r;
    
  }
  array_header * arr = _alloc0(v + sizeof(array_header));
  arr->check = 0x123;
  
  arr->next = ctx->array_pool[poolid].occupied_arrays;

  ctx->array_pool[poolid].occupied_arrays = arr;
  allocated += v;
  return arr + 1;

}

void * lisp_realloc(void * p, size_t v){
  return realloc(p, v);
}

lisp_value lisp_get_allocated(){
  return integer(allocated);
}


void * lisp_malloc_atomic(size_t v){
  return lisp_malloc(v);
  //return GC_malloc_atomic(v);
}

void lisp_free(void * p){
  //free(p);
}

lisp_value lisp_count_allocated(){
  var gc = current_context->gc;
  size_t allocated = 0;
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    allocated += buf->size;
    var free = buf->free_cons;
    while(free){
      allocated -= 1;
      free = free->cdr.cons;
    }
    buf = buf->next;
  }
  array_pool * array_pools = gc->array_pool;
  for(size_t i = 0; i < 3 * 1024 + 1; i++){
    var ptr = array_pools[i].occupied_arrays;
    while(ptr){
      allocated += i;
      array_header * next = ptr;
      ptr = next->next;
    }
  }
  return integer(allocated);

}
