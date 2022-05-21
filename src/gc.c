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

int allccc = 0;
static void * _alloc(size_t v){
  allccc += 1;
  if(allccc == 30000)
    raise(SIGINT);
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

typedef struct __array_pool array_pool;
struct __array_pool{
  void * free_arrays;
  void * occupied_arrays; 
};

struct __gc_context{
  cons_buffer * cons_pool;
  array_pool array_pool[3 * 1024 + 1];
};

gc_context * gc_context_new(){
  gc_context * ctx = _alloc0(sizeof(*ctx));
  return ctx;
}

static bool mark_cons(gc_context * gc, cons * c){
  cons_buffer * pool = gc->cons_pool;
  while(pool){
    size_t offset = c - pool->buffer;
    if(offset < pool->size){
      if(pool->gc_mark[offset])
        return false;

      pool->gc_mark[offset] = true;
      return true;
    }
    pool = pool->next;
  }
  ASSERT(false);
  return false;
}

static bool mark_vector(gc_context * gc, void * vector){
  size_t * mark = vector - sizeof(void*);
  if(*mark)
    return false; // already marked.
  *mark = true;
  return true;
}
void iterate_value(void * key, void * value, void * data);
void iterate_scope(gc_context * ctx, lisp_scope * scope);

void visit_value(gc_context * gc, lisp_value val){
  
  switch(val.type){
  case LISP_CONS:
    
    if(is_heap_ptr(val.cons) && !mark_cons(gc, val.cons)) 
      return;
    visit_value(gc, car(val));
    visit_value(gc, cdr(val));
    return;
    
  case LISP_FUNCTION_MACRO:
  case LISP_FUNCTION:
    if(is_heap_ptr(val.function) && !mark_vector(gc, val.function))
      return;
    
    if(is_heap_ptr(val.function)){
      if(mark_vector(gc, val.function->closure)){
        iterate_scope(gc, val.function->closure);
      }
    }else{
      iterate_scope(gc, val.function->closure);
    }
    visit_value(gc, val.function->code);
    visit_value(gc, val.function->args);
    return;
  case LISP_ALIEN_FUNCTION:
    if(is_heap_ptr(val.alien_func) && !mark_vector(gc, val.alien_func))
      return;
    visit_value(gc, val.alien_func->return_example);
    visit_value(gc, val.alien_func->arg_example);
    return;    
  case LISP_FUNCTION_NATIVE:
    if(is_heap_ptr(val.nfunction) && !mark_vector(gc, val.nfunction))
      return;
    return;
  case LISP_HASHTABLE:
    {
      hash_table * table = val.native_pointer;
      if(!mark_vector(gc, table)) return;
      ht_iterate(table, iterate_value, gc);
      
      mark_vector(gc, table->keys);
      mark_vector(gc, table->elems);
      mark_vector(gc, table->occupied);
    }
    return;
  case LISP_SCOPE:
    {
      if(is_heap_ptr(val.scope) && !mark_vector(gc, val.scope))
        return;
      iterate_scope(gc, val.scope);
      break;
    }
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
    case LISP_ALIEN_FUNCTION:
    case LISP_SCOPE:
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
    return;
  }
}

void iterate_value(void * key, void * value, void * data){
  lisp_value * val = value;
  gc_context * gc = data;
  visit_value(gc, *val); 
}

void clear_gc_marks(gc_context * gc){
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    memset(buf->gc_mark, 0, buf->size * sizeof(buf->gc_mark[0]));

    buf = buf->next;
  }
  
  array_pool * array_pools = gc->array_pool;
  for(size_t i = 0; i < 3 * 1024 + 1; i++){
    var ptr = array_pools[i].occupied_arrays;
    while(ptr){
      void ** markloc = ptr;
      markloc[1] = NULL;
      void ** next = ptr;
      ptr = *next;
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
    void ** place = &array_pools[i].occupied_arrays;
    while(*place){
      void * obj = *place;
      void ** header = obj;
      
      if(!header[1]){
        //printf("Scope? %p\n", obj + 16);
        void * next = header[0];
        header[0] = pool->free_arrays;
        pool->free_arrays = obj;
        *place = next;

      }else
        place = header;
    }
  }
}

void iterate_scope(gc_context * ctx, lisp_scope * scope){
  //printf("Scope %p!\n", scope);
  if(scope->values != NULL)
    ht_iterate(scope->values, iterate_value, ctx);
  var cns = scope->lookup;
  for(size_t i = 0; i < scope->argcnt; i++){
    visit_value(ctx, cns[i].cdr);
    visit_value(ctx, cns[i].car);
  }
  if(scope->super == NULL) return;
  if(is_heap_ptr(scope->super) && !mark_vector(ctx, scope->super))
    return;
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
void gc_collect_garbage(lisp_context * context){
  var gc_context = context->gc;
  clear_gc_marks(gc_context);
  iterate_scope(gc_context, context->globals);
  gc_recover_unmarked(gc_context);
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
      printf("new pool\n");
      cons_buffer ** parent = &ctx->cons_pool;
      while(*parent != NULL){
        pool_size *= 4;
        parent = &(*parent)->next;
      }
      
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
  
  if(current_context == NULL) return _alloc(v);
  var ctx = current_context->gc;
  
  size_t poolid = 0;
  if(v < 1024){
    // small_pool
    poolid = v;
    v += 1;
  }else if(v < 1024 * 1024){
      poolid = v / 1024 + 1024;
      v += 1024;
  }else if(v < 1024 * 1024 * 1024){
    poolid = v / (1024 * 1024) + 1024 + 1024;
    v += 1024 * 1024;
  }else {
    poolid = 1024 + 1024 + 1024;
  }

  if(ctx->array_pool[poolid].free_arrays){
    //printf("reuse free array\n");
    var new_ptr = ctx->array_pool[poolid].free_arrays;
    void ** next_ptr = (void **) new_ptr;
    ctx->array_pool[poolid].free_arrays = *next_ptr;
    next_ptr[0] = ctx->array_pool[poolid].occupied_arrays;
    ctx->array_pool[poolid].occupied_arrays = next_ptr;
    return new_ptr + sizeof(void*) * 2;
  }
  void * arr = _alloc(v + sizeof(void *) * 2);
  
  void ** arr_base = arr;
  arr_base[0] = ctx->array_pool[poolid].occupied_arrays;
  arr_base[1] = NULL; // gc mark
  ctx->array_pool[poolid].occupied_arrays = arr;
  allocated += v;
  return arr + sizeof(void*) * 2;

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
      void ** next = ptr;
      ptr = *next;
    }
  }
  return integer(allocated);

}
