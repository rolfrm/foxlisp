#include <iron/full.h>
#include "foxlisp.h"
extern lisp_context * current_context;

void test_gc(){
  var ctx = lisp_context_new_bare();
  current_context = ctx;

  { // test cons
    var s1 = get_symbol("a");
    var c2 = new_cons(nil, nil);
    var c1 = new_cons(c2, nil);
    gc_clear(current_context->gc);
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c2)));
    gc_mark(current_context);

    // this cons is not marked, because it is not referenced from anywhere.
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c2)));
  
    gc_mark_cons(ctx->gc, lisp_value_cons(c1));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c2)));
    
    gc_clear(current_context->gc);
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c2)));
  
    // now inserting the value into the context, should cause it to be marked during gc.
    lisp_scope_create_value(ctx->globals, s1, c1);
    gc_mark(current_context);
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c2)));
  }
  if(true){ // test arrays
    for(int j = 0; j < 20; j++){
      int count = 13 * (j + 1);
    for(int i = 0 ; i < 10; i++){
      printf("allocated: ");println(lisp_get_allocated());
      var s2 = get_symbol("b");
      
      int * x = lisp_malloc(sizeof(int) * count);
      int * y = lisp_malloc(sizeof(int) * count);
      for(int k = 0; k < 5; k++){
        ASSERT(x != lisp_malloc(sizeof(int) * count));
        ASSERT(y != lisp_malloc(sizeof(int) * count));
      }
      ASSERT(x != y);
      for(int i = 0; i < count; i++){
        ASSERT(x[i] == 0);
        ASSERT(y[i] == 0);
      }
      for(int i = 0; i < count; i++){
        x[i] = i;
        y[i] = i * 2;
      }
      
      ASSERT(!vector_is_marked(ctx->gc, x));
      gc_mark(current_context);
      ASSERT(!vector_is_marked(ctx->gc, x));
      
      var ptr = allocated_pointer_lisp_value(x);
      printf("ptr: %p\n", ptr.native_pointer);
      lisp_scope_create_value(ctx->globals, s2, ptr);
      gc_clear(current_context->gc);
      gc_mark(current_context);
      ASSERT(vector_is_marked(ctx->gc, x));
      gc_recover_unmarked(ctx->gc);
    }
    }
    //lisp_scope_create_value(ctx->globals, s2, x);
    //gc_clear(current_context->gc);
    //gc_mark(current_context);
    //ASSERT(vector_is_marked(ctx->gc, x));
    
  }
  {
     // test hashtable related GCing
    var s2 = get_symbol("c");

    var ht = lisp_make_hashtable();
    var c2 = new_cons(nil, nil);
    var c1 = new_cons(c2, nil);
    var c5 = new_cons(nil, nil);
    lisp_scope_create_value(ctx->globals, s2, ht);
    gc_clear(current_context->gc);
    gc_mark(current_context);
    
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c2)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c5)));

    lisp_hashtable_set(ht, c1, nil);
    lisp_hashtable_set(ht, c1, c5);
    gc_clear(current_context->gc);
    gc_mark(current_context);
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c2)));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c5)));

    lisp_hashtable_set(ht, c1, nil);
    gc_clear(current_context->gc);
    gc_mark(current_context);
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c5)));
    
    gc_collect_garbage(ctx);

    var c3 = new_cons(nil, nil);
    var c4 = new_cons(c3, nil);
    gc_clear(current_context->gc);
    gc_mark(current_context);
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c1)));
    ASSERT(cons_is_marked(ctx->gc, lisp_value_cons(c2)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c3)));
    ASSERT(!cons_is_marked(ctx->gc, lisp_value_cons(c4)));
    gc_clear(current_context->gc);
    gc_collect_garbage(ctx);
    
    var t2 = lisp_value_hashtable(ht);
    printf("??\n");
    var t1 = lisp_malloc(sizeof(*t2));
    printf("%p %p %i \n", t1, t2, (i32)sizeof(*t2));
    printf("%p %p %p\n", t2->keys, t2->elems, t2->occupied);
    ASSERT(t1 != t2);
    gc_collect_garbage(ctx);

    for(int i = 0; i < 100; i++){
      char buf[10] = {0};
      sprintf(buf, "S%i", i);
      lisp_hashtable_set(ht, get_symbol(buf), nil);
      printf(" %i\n", (i32)lisp_value_integer(lisp_hashtable_count(ht)));
      gc_collect_garbage(ctx);

    }
    //lisp_hashtable_set(ht, c2, nil);
        //lisp_malloc(sizeof(hash_table));
        
  }
  
}
