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

typedef struct{
  lisp_context * ctx;
  hash_table * visited_nodes;

}gc_context;
void iterate_scope(gc_context * ctx, lisp_scope * scope);

void visit_value(gc_context * gc, lisp_value val){
  println(val);
  switch(val.type){
  case LISP_CONS:
    if(!ht_set(gc->visited_nodes, &val.cons, NULL)){
      return;
    }
    printf("new cons\n");
    visit_value(gc, val.cons->car);
    visit_value(gc, val.cons->cdr);
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
