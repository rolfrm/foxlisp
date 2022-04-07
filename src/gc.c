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

typedef struct _cons_buffer cons_buffer;

struct _cons_buffer {
  cons_buffer * next;
  cons * free_cons;
  cons * buffer;
  bool * gc_mark;
};

typedef struct __array_pool array_pool;
struct __array_pool{
  void * free_arrays;
  void * occupied_arrays; 
};

struct __gc_context{
  lisp_context * ctx;
  hash_table * visited_nodes;
  cons_buffer * cons_pool;
  array_pool array_pool[3 * 1024 + 1];
};

gc_context * gc_context_new(){
  gc_context * ctx = malloc(sizeof(*ctx));
  *ctx = (gc_context){0};
  return ctx;
}

void iterate_scope(gc_context * ctx, lisp_scope * scope);

void visit_value(gc_context * gc, lisp_value val){
  println(val);
  switch(val.type){
  case LISP_CONS:
    if(!ht_set(gc->visited_nodes, &val.cons, NULL)){
      return;
    }
    
    visit_value(gc, car(val));
    visit_value(gc, cdr(val));
    return;
    
  case LISP_FUNCTION_MACRO:
  case LISP_FUNCTION:
    if(!ht_set(gc->visited_nodes, &val.function, NULL))
      return;
    iterate_scope(gc, val.function->closure);
    visit_value(gc, val.function->code);
    visit_value(gc, val.function->args);
    return;
  case LISP_ALIEN_FUNCTION:
    ht_set(gc->visited_nodes, &val.alien_func, NULL);
    visit_value(gc, val.alien_func->return_example);
    visit_value(gc, val.alien_func->arg_example);
    return;    
  case LISP_FUNCTION_NATIVE:
    ht_set(gc->visited_nodes, &val.nfunction, NULL);
    return;

  case LISP_VECTOR:
    if(!ht_set(gc->visited_nodes, &val.vector, NULL))
      return;
    
    switch (val.vector->default_value.type){
    case LISP_INTEGER:
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
    ht_set(gc->visited_nodes, val.string, NULL);
    return;
  case LISP_NATIVE_POINTER:
  case LISP_MACRO_BUILTIN:
  case LISP_INTEGER:
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
  
  int * sym = key;
  char * sym_string = (char *) symbol_name(*sym);
  
  lisp_value * val = value;
  printf("-- %p %s\n", val, sym_string);
  gc_context * gc = data;
  visit_value(gc, *val);
  
}

void iterate_scope(gc_context * ctx, lisp_scope * scope){
  printf("Scope %p!\n", scope);
  if(scope->values != NULL)
    ht_iterate(scope->values, iterate_value, ctx);
}
size_t ht_count(hash_table * ht);
void gc_collect_garbage(lisp_context * context){
  gc_context ctx;
  ctx.ctx = context;
  ctx.visited_nodes = ht_create2(1024, sizeof(void *), 0);
  iterate_scope(&ctx, context->globals);
  printf("Counted %i objects\n", ht_count(ctx.visited_nodes));
  
  ht_clear(ctx.visited_nodes);
}

lisp_context * current_context;

lisp_value new_cons(lisp_value _car, lisp_value _cdr){
  var ctx = current_context->gc;
   while(true){
    size_t pool_size = 1024;
    var pool = ctx->cons_pool;
    while(pool != NULL){
      if(pool->free_cons != NULL){
        
        var found = pool->free_cons;
        pool->free_cons = pool->free_cons->cdr.cons;
        memset(found, 0, sizeof(*found));
        found->car = _car;
        found->cdr = _cdr;
        return (lisp_value){.type = LISP_CONS, .cons = found};
      }else{
        pool = pool->next;
        pool_size *= 4;
      }
    }
    if(pool == NULL){
      cons_buffer * new_pool = alloc0(sizeof(*pool));
      new_pool->buffer = alloc0(sizeof(cons) * pool_size);
      new_pool->free_cons = new_pool->buffer;
      
      for(size_t i = 0; i < pool_size - 1; i++){
        new_pool->buffer[i].cdr.cons = &new_pool->buffer[i + 1];
        new_pool->buffer[i].car = nil;                 
      }
      size_t pool_size2 = 1024/4;
      var parent = &ctx->cons_pool;
      while(pool_size2 * 4 != pool_size){
        parent = &((*parent)->next);
        pool_size2 *= 4;
      }
      *parent = new_pool;
      
    }
  }
  // this hsould never happen.
  return nil;
}

void * gc_clone(const void * mem, size_t s){
  void * d = lisp_malloc(s);
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
  if(current_context == NULL) return malloc(v);
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
    var new_ptr = ctx->array_pool[poolid].free_arrays;
    void ** next_ptr = (void **) new_ptr;
    ctx->array_pool[poolid].free_arrays = *next_ptr;
    return new_ptr + sizeof(void*);
  }
  void * arr = malloc(v + sizeof(void *));
  void ** arr_base = arr;
  arr_base[0] = ctx->array_pool[poolid].occupied_arrays;
  ctx->array_pool[poolid].occupied_arrays = arr;
  allocated += v;
  return arr + sizeof(void*);

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
  size_t cbs = 1024;
  var buf = gc->cons_pool;
  
  while(buf != NULL){
    allocated += cbs;
    cbs *= 3;
    buf = buf->next;
  }
  array_pool * array_pools = gc->array_pool;
  for(size_t i = 0; i < 3 * 1024 + 1; i++){
    var ptr = array_pools[i].occupied_arrays;
    while(ptr){
      allocated += 1;
      void ** next = ptr;
      ptr = *next;
    }
  }
  return integer(allocated);

}
