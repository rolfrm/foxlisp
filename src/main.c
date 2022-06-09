#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>

#ifdef EMSCRIPTEN
#include "emscripten.h"
#endif

#include "foxlisp.h"
lisp_value lisp_read_stream(io_reader * rd);
#include "lisp_value2.c"

extern bool debug_set;
bool tracing = false;


lisp_value rest_sym = {.type = LISP_SYMBOL};
lisp_value if_sym = {.type = LISP_SYMBOL};
lisp_value quote_sym = {.type = LISP_SYMBOL};
lisp_value quasiquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_splice_sym = {.type = LISP_SYMBOL};
lisp_value nil = {0};
lisp_value t = {.type = LISP_T};
lisp_value else_sym;
lisp_value otherwise_sym;

#undef ASSERT
void print_call_stack();
#define lisp_eval2 lisp_eval
static __thread lisp_value current_error;
static inline bool lisp_is_in_error(){
  return !is_nil(current_error);
}

void raise_string(const char * str){
  current_error = string_lisp_value(str);
  printf("%s\n", str);
  print_call_stack();
}

lisp_value lisp_error(lisp_value value){
  current_error = value;
  println(current_error);
  print_call_stack();
  return nil;
}

bool type_assert(lisp_value val, lisp_type type){
  if(lisp_value_type(val) != type){
    char buffer[1000];
	 sprintf(buffer, "Invalid type, expected %s, but got %s\n",
            lisp_type_to_string(type),
            lisp_type_to_string(lisp_value_type(val)));
	 raise_string(nogc_clone(buffer, strlen(buffer) + 1));
    return false;
  }
  return true;
}

bool elem_type_assert(lisp_value vector, lisp_type type){
  return type_assert(lisp_value_vector(vector)->default_value, type);
}

#define ASSERT(x) if(!(x)){raise_string("!!! " #x "\n"); }

bool _string_eq(lisp_value a, lisp_value b){
  var a_type = lisp_value_type(a);
  if(a_type != lisp_value_type(b)) return false;
  if(a_type != LISP_STRING) return false;
  var as = lisp_value_string(a);
  var bs = lisp_value_string(b);
  if(strlen(as) != strlen(bs)) return false;
  return strncmp(as, bs, strlen(as)) == 0;
}

lisp_value string_eq(lisp_value a, lisp_value b){
  return bool_lisp_value(_string_eq(a, b));
}

lisp_value lisp_is_list(lisp_value a){
  return bool_lisp_value(is_nil(a) || is_cons(a));
}

lisp_value lisp_is_cons(lisp_value a){
  return bool_lisp_value(is_cons(a));
}

lisp_scope * lisp_scope_new(lisp_scope * super){
  lisp_scope * s = lisp_malloc(sizeof(*super));
  *s = (lisp_scope){0};
  s->super = super;
  s->values = NULL;
  return s;
}

void lisp_scope_stack(lisp_scope * s, lisp_scope * super, cons * lookup, size_t cnt){
  s->super = super;
  s->values = NULL;
  s->lookup = lookup;
  s->stack_scope = true;
  s->lookup_on_stack = true;
  s->argcnt = cnt;
}

// since symbol IDs are forthrunning numbers anyway, there is not really a good reason to hash them.
// experiments show a slight performance increase when doing this.
static int symbol_nohash(const void * key, void * userdata){
  const u64 * k = key;
  return (int)k[0];
}

// moving a scope off the stack.
lisp_scope * lisp_scope_unstack(lisp_scope * scope){

  if(scope->super == NULL) 
    return scope; // dont unstack from the super scope.
  if(scope->stack_scope == false)
    return scope; // the scope was already unstacked.
  scope->super = lisp_scope_unstack(scope->super);
  if(scope->lookup_on_stack){
    scope->lookup_on_stack = false;
    scope->lookup = gc_clone(scope->lookup, sizeof(scope->lookup[0]) * scope->argcnt);
    
  }
  var newscope = (lisp_scope *) gc_clone(scope, sizeof(*scope));
  newscope->stack_scope = false;
  return newscope;
}

lisp_value lisp_scope_get_value(lisp_scope * scope, lisp_value sym){
  lisp_value val;
  if(lisp_scope_try_get_value(scope, sym, &val))
    return val;
  return nil;
}

bool _lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out, size_t hash){
  if(scope == NULL) return false;
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(is_nil(scope->lookup[i].car))
        break;
      if(lisp_value_eq(scope->lookup[i].car, sym)){
        *out = scope->lookup[i].cdr;
        return true;
      }
    }
  }
  int index;
  if(scope->values != NULL && ht_get_precalc(scope->values_index, hash, &sym.integer, &index)){
    *out = scope->values[index];
	 return true;
  }
  return _lisp_scope_try_get_value(scope->super, sym, out, hash);
}

bool lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out){
  //printf("Slow fetch: "); println(sym);
  if(scope == NULL) return false;
  if(scope->lookup != NULL){
    var argcnt = scope->argcnt;
    for(size_t i = 0; i < argcnt; i++){
      var v = lisp_value_symbol(scope->lookup[i].car);
      if(v == 0) break;
      if(v == lisp_value_symbol(sym)){
        
        *out = scope->lookup[i].cdr;
        return true;
      }
    }
  }
  if(scope->values == NULL)
    return lisp_scope_try_get_value(scope->super, sym, out);
  lisp_symbol i = lisp_value_symbol(sym);
  size_t hash = ht_calc_hash(scope->values_index, &i);
  return _lisp_scope_try_get_value(scope, sym, out, hash);
}

bool lisp_scope_try_get_value2(lisp_scope * scope, i64 sym, lisp_scope ** out, int * index, int * scope_index){
  *index = -1;
  *scope_index = -1;
  *out = NULL;
 start:
  if(scope == NULL) return false;
  if(scope->lookup != NULL){
    var argcnt = scope->argcnt;
    for(size_t i = 0; i < argcnt; i++){
      var v = lisp_value_symbol(scope->lookup[i].car);
      if(v == 0) break;
      if(v == sym){
        *scope_index = i;
        *out = scope;
        return true;
      }
    }
  }
  if(scope->values != NULL && ht_get(scope->values_index, &sym, index)){
    *out = scope;
    return true;
  }
  scope = scope->super;
  goto start;
  return false;
    
}


lisp_value lisp_scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope == NULL){
	 raise_string("Variable not found");
	 return nil;
  }
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(is_nil(scope->lookup[i].car))
        break;
      if(lisp_value_eq(scope->lookup[i].car, sym)){
        scope->lookup[i].cdr = value;
        return t;
      }
    }
  }
  int index;
  lisp_symbol i = lisp_value_symbol(sym);
  if(scope->values != NULL && ht_get(scope->values_index, &i, &index)){
	 scope->values[index] = value;
	 return t;
  }
  return lisp_scope_set_value(scope->super, sym, value);
}

lisp_value lisp_scope_create_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(is_nil(scope->lookup[i].car)){
        scope->lookup[i] = (cons){.car = sym, .cdr = value};
        return nil;
      }
    }
  }
  
  if(scope->values == NULL){
	 scope->values_index = ht_create2(2048, sizeof(u64), sizeof(int));
    scope->values_index->hash = symbol_nohash;
    scope->values_capacity = 2048;
    scope->values = malloc(scope->values_capacity * sizeof(lisp_value));
    scope->values[0] = nil;
    scope->values_count = 1;
  }
  
  if(scope->values_count == scope->values_capacity){
    scope->values_capacity *= 2;
    scope->values = realloc(scope->values, scope->values_capacity * sizeof(lisp_value)); 
  }
  
  int idx = scope->values_count;
  lisp_symbol i = lisp_value_symbol(sym);
  ht_set(scope->values_index, &i, &idx);
  scope->values[scope->values_count] = value;
  scope->values_count += 1;
  return nil;
}

lisp_context * current_context;
void lisp_push_scope(lisp_scope * scope){
  if(current_context->scope_count == current_context->scope_capacity){
    current_context->scope_capacity = MAX(8, current_context->scope_capacity * 2);
    current_context->scopes = realloc(current_context->scopes, sizeof(current_context->scopes[0]) * current_context->scope_capacity);
  }
  current_context->scopes[current_context->scope_count] = scope;
  current_context->scope_count += 1;
}

void lisp_pop_scope(lisp_scope * scope){
  ASSERT(current_context->scope_count > 0);
  ASSERT(current_context->scopes[current_context->scope_count - 1] == scope);
  current_context->scopes[current_context->scope_count - 1] = NULL;
  current_context->scope_count -= 1; 
}

lisp_context * lisp_context_new(){
  printf("LISP NEW CONTEXT\n");
  lisp_context * ctx = lisp_malloc(sizeof(ctx[0]));
  ctx->gc = gc_context_new();
  ctx->next_symbol = 1;
  ctx->symbols = ht_create_strkey(sizeof(u64));
  ctx->symbols_reverse = ht_create(sizeof(u64), sizeof(char *));
  ctx->symbols_reverse->hash = symbol_nohash;
    
  ctx->globals = lisp_scope_new(NULL);
  var prev_ctx = current_context;
  current_context = ctx;
  rest_sym = get_symbol("&rest");
  if_sym = get_symbol("if");
  quote_sym = get_symbol("quote");
  quasiquote_sym = get_symbol("quasiquote");
  unquote_sym = get_symbol("unquote");
  else_sym = get_symbol("else");
  otherwise_sym = get_symbol("otherwise");
  unquote_splice_sym = get_symbol("unquote-splicing");
  lisp_scope_create_value(current_context->globals, get_symbol("nil"), nil);
  lisp_scope_create_value(current_context->globals, get_symbol("t"), t);
  current_context = prev_ctx;
  
  return ctx;
}

inline lisp_value car(lisp_value v){
  if(is_cons(v))
	 return lisp_value_cons(v)->car;
  return nil;
}

inline lisp_value cdr(lisp_value v){
  if(is_cons(v))
	 return lisp_value_cons(v)->cdr;
  return nil;
}

lisp_value cddr(lisp_value v){
  return cdr(cdr(v));
}

lisp_value cadr(lisp_value v){
  return car(cdr(v));
}

lisp_value set_cdr(lisp_value cons, lisp_value value){
  TYPE_ASSERT(cons, LISP_CONS);
  var c = lisp_value_cons(cons);
  var old = c->cdr;
  c->cdr = value;
  return old;
}

lisp_value set_car(lisp_value cons, lisp_value value){
  TYPE_ASSERT(cons, LISP_CONS);
  var c = lisp_value_cons(cons);
  var old = c->car;
  c->car = value;
  return old;
}

size_t list_length(lisp_value lst){
  size_t l = 0;
  while(is_cons(lst)){
	 l += 1;
	 lst = cdr(lst);
  }
  return l;
}

lisp_value _lisp_append(lisp_value v1, lisp_value v2){
  if(is_nil(v1))
	 return v2;
  return new_cons(car(v1), _lisp_append(cdr(v1), v2));
}

lisp_value lisp_append(lisp_value v1, lisp_value v2){
  if(is_nil(v2))
	 return v1;
  return _lisp_append(v1, v2);
}

lisp_value lisp_length(lisp_value lst){
  return integer_lisp_value(list_length(lst));
}

lisp_value copy_cons(lisp_value a){
  if(is_cons(a))
    return new_cons(car(a), copy_cons(cdr(a)));
  return a;
}

lisp_value copy_cons_deep(lisp_value a){
  if(is_cons(a))
    return new_cons(copy_cons(car(a)), copy_cons(cdr(a)));
  return a;
}

int64_t get_symbol_id(const char * s){
  int64_t id = 0;
  if(ht_get(current_context->symbols, &s, &id))
    return id;
  if(s == NULL){
    raise_string("null symbol");
    return 0;
  }
    
  s = nogc_clone(s, strlen(s) + 1);
  id = current_context->next_symbol++;
  if(s[0] == ':'){
    id += 0x10000000;
  }
  ht_set(current_context->symbols, &s, &id);
  ht_set(current_context->symbols_reverse, &id, &s);
  return id;
}

lisp_value get_symbol(const char * s){
  if(s == NULL || strlen(s) == 0){
    raise_string("Symbol empty\n");
  }
  return symbol_lisp_value(get_symbol_id(s));
}

const char * symbol_name(int64_t id){
  char * out;
  if(ht_get(current_context->symbols_reverse, &id, &out))
	 return out;
  return NULL;
}

lisp_value string_to_symbol(lisp_value string){
  TYPE_ASSERT(string, LISP_STRING);
  return get_symbol(lisp_value_string(string));
}

lisp_value symbol_to_string(lisp_value sym){
  TYPE_ASSERT(sym, LISP_SYMBOL);
  char * sym_string = (char *) symbol_name(lisp_value_symbol(sym));
  return string_lisp_value(sym_string);
}

lisp_value string_starts_with(lisp_value str, lisp_value str2){
  TYPE_ASSERT(str, LISP_STRING);
  TYPE_ASSERT(str2, LISP_STRING);
  var astr = lisp_value_string(str);
  var bstr = lisp_value_string(str2);
  return bool_lisp_value(strncmp(astr, bstr, strlen(bstr)) == 0);
}


lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value);

lisp_value lisp_sub_macro_expand(lisp_scope * scope, lisp_value c){
  if(is_nil(c)) return nil;
  var current = car(c);
  var next = cdr(c);
  var value = lisp_macro_expand(scope, current);
  if(!is_nil(next)){
	 var nextr = lisp_sub_macro_expand(scope, next);
	 if(lisp_value_eq(current, value) && lisp_value_eq(next, nextr))
		return c;
	 return new_cons(value, nextr);
  }else{
	 if(lisp_value_eq(current, value))
		return c;
	 return new_cons(value, nil);
  }
}

lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value){
  if(!is_cons(value))
	 return value;
  if(lisp_value_eq(car(value), quote_sym)) return value;
  var value_new = lisp_sub_macro_expand(scope, value);
  if(!lisp_value_eq(value_new, value))
	 return value_new;
  lisp_value head = car(value);
  if(!is_symbol(head)) return value;
  if(lisp_value_eq(head, quote_sym)) return value;
  lisp_value head_value = lisp_scope_get_value(scope, head);
  if(!is_function_macro(head_value)) return value;
  lisp_function * f = lisp_value_function(head_value);
  let argcnt = list_length(f->args);
  cons args3[argcnt];
  memset(args3, 0, sizeof(args3[0]) * argcnt);
  lisp_scope function_scope[1] = {0};
  lisp_scope_stack(function_scope, f->closure, args3, argcnt);
  lisp_push_scope(function_scope);
  var args = f->args;
  var args2 = cdr(value);
  while(!is_nil(args)){
	 var arg = car(args);
	 var argv = car(args2);
	 
	 if(!is_symbol(arg)){
		println(arg);
		println(value);
		println(args);
		println(args2);
		raise_string("arg name must be a symbol\n");
      lisp_pop_scope(function_scope);
		return nil;
	 }
	 if(lisp_value_eq(arg, rest_sym)){
		args = cdr(args);
		arg = car(args);
		if(!is_symbol(arg)){
		  raise_string("(2) arg name must be a symbol.");
        lisp_pop_scope(function_scope);
		  return nil;
		}
		lisp_scope_create_value(function_scope, arg, args2);
		break;
	 }
	 lisp_scope_create_value(function_scope, arg, argv);
	 
	 args = cdr(args);
	 args2 = cdr(args2);
	 
  }
  		
  var it = f->code;
  lisp_value ret = nil;
  while(!is_nil(it)){
	 ret = lisp_eval(function_scope, car(it));
	 it = cdr(it);
  }
  lisp_pop_scope(function_scope);
		  
  return ret;
}

lisp_value lisp_sub_eval(lisp_scope * scope, lisp_value c, cons * cons_buf){
  if(is_nil(c)) return nil;

  var next = cdr(c);
  lisp_value cns = cons_lisp_value(cons_buf);
	 
  if(!is_nil(next)){

	 var value = lisp_eval2(scope, car(c));
	 var nextr = lisp_sub_eval(scope, next, cons_buf + 1);
    set_car(cns, value);
    set_cdr(cns, nextr);
  }else{
    set_car(cns, lisp_eval2(scope, car(c)));
    set_cdr(cns, nil);
  }
  return cns;
}

lisp_value lisp_eval_quasiquoted(lisp_scope * scope, lisp_value value);

lisp_value lisp_eval_quasiquoted_sub(lisp_scope * scope, lisp_value value){
  if(is_nil(value)) return value;
  var current = car(value);
  var next = cdr(value);
  bool unsplice = lisp_value_eq(car(current), unquote_splice_sym);
  var value2 = lisp_eval_quasiquoted(scope, current);
  if(!is_nil(next)){
	 var nextr = lisp_eval_quasiquoted_sub(scope, next);
	 if(lisp_value_eq(current, value2) && lisp_value_eq(next, nextr))
		return value;
	 if(unsplice)
		return lisp_append(value2, nextr);
	 
	 return new_cons(value2, nextr);
  }else{
	 if(lisp_value_eq(current, value2))
		return value;
	 if(unsplice)
		return value2;
	 return new_cons(value2, nil);
  }
}

lisp_value lisp_eval_quasiquoted(lisp_scope * scope, lisp_value value){
  switch(lisp_value_type(value)){
  case LISP_CONS:
	 {
		var fst = car(value);
      
		if(lisp_value_eq(fst, unquote_sym))
		  return lisp_eval(scope, cadr(value));
		if(lisp_value_eq(fst, unquote_splice_sym))
		  return lisp_eval(scope, cadr(value));
		  
		return lisp_eval_quasiquoted_sub(scope, value);
	 }
  default:
	 return value;
  }
}

void print_call_stack(){
}

lisp_value lisp_collect_garbage(){
  gc_collect_garbage(current_context);
  return nil;
}

static inline size_t lisp_optimize_statement(lisp_scope * scope, lisp_value statement){
  size_t c = 0;
  while(is_cons(statement)){
    var first = car(statement);
    if(is_symbol(first)){
      lisp_scope * s1;
      int i1, i2;
      if(lisp_scope_try_get_value2(scope, lisp_value_symbol(first), &s1, &i1, &i2)){
        
        if(s1 == current_context->globals){
          lisp_value new = global_index_lisp_value(i1);
          set_car(statement, new);
        }else{
          int cnt = 0;
          var scope2 = scope;
          while(scope2 != s1){
            cnt += 1;
            scope2 = scope2->super;
          }
          
          lisp_value new = local_index_lisp_value(cnt,i1 == -1 ? i2 : i1, i2 == -1);
          set_car(statement, new);
        }
      }
    }
    statement = cdr(statement);
    c += 1;
  }
  return c;
}

lisp_value lisp_eval(lisp_scope * scope, lisp_value value){
 tail_call:;
  switch(lisp_value_type(value)){
  case LISP_CONS:
	 {
      
      var first = car(value);
      lisp_value first_value;

      lisp_scope * s1;
      int i1, i2;
      if(is_symbol(first) && lisp_scope_try_get_value2(scope, first.integer, &s1, &i1, &i2))
        {
          if(i2 == -1){
            first_value = s1->values[i1];
          }else{
            first_value = s1->lookup[i2].cdr;
          }
          if(s1 == current_context->globals){
            lisp_value new = global_index_lisp_value(i1);
            set_car(value, new);
            
            var first_value2 = lisp_eval2(scope, new);
            ASSERT(lisp_value_eq(first_value, first_value2));
            first_value = first_value2;
          }
          // this is a performance optimization for the normal case - calling a function by name
        }else{
        first_value = lisp_eval2(scope, first);
      }

		if(is_macro_builtin(first_value)){
		  switch(first_value.builtin){
		  case LISP_IF:
			 {
				var cond = lisp_eval2(scope, cadr(value));
            if(cond.type == LISP_NIL)
				  value = cadddr(value);
				else
				  value = caddr(value);
            goto tail_call;
			 }
		  case LISP_QUOTE:
			 if(first_value.builtin == LISP_QUOTE)
				return cadr(value);
		  case LISP_QUASIQUOTE:
			 return lisp_eval_quasiquoted(scope, cadr(value));
		  case LISP_UNQUOTE_SPLICE:
		  case LISP_UNQUOTE:
			 raise_string("Unexpected unquote!\n");
			 break;
         
		  case LISP_LET:
			 {

				var argform = cadr(value);
            var argcnt = list_length(argform);
            cons argsbuf[argcnt];
            memset(argsbuf, 0, sizeof(argsbuf[0]) * argcnt);
            lisp_scope scope1[1] = {0};
            lisp_scope_stack(scope1, scope, argsbuf, argcnt);
          
            scope = scope1;
            while(!is_nil(argform)){

              var arg = car(argform);
				  var sym = car(arg);
				  var value = lisp_eval(scope, cadr(arg));
				  if(lisp_is_in_error()) return nil;
              lisp_scope_create_value(scope, sym, value);
              argform = cdr(argform);
				}
				value = cdr(value);
            var body = cdr(value);
				lisp_value result = nil;
            while(!is_nil(body)){
				  result = lisp_eval(scope, car(body));
				  if(lisp_is_in_error()) break;
				  body = cdr(body);
				}
				return result;
			 }
		  case LISP_PROGN:
			 {
				var body = cdr(value);
				lisp_value result = nil;
				while(!is_nil(body)){
				  result = lisp_eval2(scope, car(body));
              if(lisp_is_in_error()) break;
				  body = cdr(body);
				}
				return result;
			 }
		  case LISP_LOOP:
			 {
				var cond = cadr(value);
				var _body = cddr(value);
            
				lisp_value result = nil;
				while(!is_nil(lisp_eval2(scope, cond))){
				  var body = _body;;
				  while(!is_nil(body)){
					 result = lisp_eval2(scope, car(body));
                if(lisp_is_in_error()) return result;
                body = cdr(body);
				  }
				}
				return result;
			 }
		  case LISP_LAMBDA:
		  case LISP_MACRO:
			 {
				var args = cadr(value);
				var body = cddr(value);
				lisp_function * f = lisp_malloc(sizeof(*f));
				f->code = body;
				f->args = args;
            scope = lisp_scope_unstack(scope);
				f->closure = scope;

				if(first_value.builtin == LISP_LAMBDA)
				  return (lisp_value){.type = LISP_FUNCTION, .function = f};
				else
				  return (lisp_value){.type = LISP_FUNCTION_MACRO, .function = f};
			 }
		  case LISP_SET:
			 {
            var ovalue = value;
            value = cdr(value);
				var sym = car(value);
            //printf("SET %s %i\n", symbol_name(sym.integer), lisp_is_in_error());
            value = cdr(value);
				var value2 = lisp_eval2(scope, car(value));
            switch(lisp_value_type(sym)){
            case LISP_SYMBOL:
              
              lisp_optimize_statement(scope, ovalue);
              lisp_scope_set_value(scope, sym, value2);
              break;
            case LISP_LOCAL_INDEX:
              while(sym.local_index.scope_level > 0){
                scope = scope->super;
                ASSERT(scope != NULL);
                sym.local_index.scope_level -= 1;
              }
              if(sym.local_index.scope_type == 0){
                cons * v = scope->lookup + sym.local_index.scope_index;
                v->cdr = value2;
              }else{
                lisp_value * v = scope->values + sym.local_index.scope_index;
                *v = value2;
              }
              break;
            case LISP_GLOBAL_INDEX:
              current_context->globals->values[sym.integer] = value2;
              break;
            default:
              raise_string("Invalid l-value");
              return nil;
            }
				
				return value2;
			 }
		  case LISP_DEFINE:
			 {
				var sym = cadr(value);
				if(!is_symbol(sym))
				  return nil; // error
				
				var value2 = lisp_eval(scope, caddr(value));
				lisp_scope_create_value(scope, sym, value2);
				return value2;
			 }
		  
        case LISP_SYMBOL_VALUE:
          {
            var sym = lisp_eval2(scope, cadr(value));
            
            if(!is_symbol(sym)){
              println(sym);
              printf("Not a symbol\n");
              return nil;
            }
            if(!is_nil(caddr(value)))
              while(scope->super != NULL)
                scope = scope->super;
            if(lisp_scope_try_get_value(scope, sym, &value))
              return value;
            return nil;
          }
        case LISP_BOUND:
          {
            var the_scope = scope;
            if(!is_nil(cddr(value)) && !is_nil(lisp_eval(scope, caddr(value)))){
              the_scope = current_context->globals;
            }
              
            var sym = lisp_eval(scope, cadr(value));
            if(!is_symbol(sym))
              return nil;
            
            if(lisp_scope_try_get_value(the_scope, sym, &value))
              return t;
            return nil;
          }
        case LISP_WITH_EXCEPTION_HANDLER:
          {
            var result = lisp_eval(scope, cadr(value));
            if(lisp_is_in_error()){
              var error = current_error;
              current_error = nil;
              var error_handler = lisp_eval(scope, caddr(value));
              var result2 = lisp_eval(scope, new_cons(error_handler, new_cons(error, nil)));
              return result2;
            }
            return result;
            
            break;
          }
        case LISP_CASE:
          {
            var result = lisp_eval2(scope, cadr(value));
            var cases = cddr(value);
            while(is_nil(cases) == false){
              var c = car(cases);
              var test = car(c);
              bool passed = false;
              if(is_cons(test)){
                while(is_cons(test)){
                  if(lisp_value_eq(result, car(test))){
                    passed = true;
                    break;
                  }
                  test = cdr(test);
                }
              }else if (lisp_value_eq(result, test)){
                passed = true;
              }else if(lisp_value_eq(test, otherwise_sym) && is_symbol(test)){
                passed = true;
              }
              
              if(passed){
                var form = cdr(c);
                lisp_value ret = nil;
                while(!is_nil(form)){
                  ret = lisp_eval(scope, car(form));
                  if(lisp_is_in_error()) return nil;
                
                  form = cdr(form);
                }
                return ret;
              }
              cases = cdr(cases);
            }
          }
          return nil;
        case LISP_COND:
          {
            var cases = cdr(value);
            while(is_nil(cases) == false){
              var c = car(cases);
              var test = car(c);

              if(lisp_value_eq(test, else_sym) || !is_nil(lisp_eval(scope, test))){
                lisp_value ret = nil;
                var form = cdr(c);
                while(!is_nil(form)){
                  ret = lisp_eval(scope, car(form));
                  if(lisp_is_in_error()) return nil;
                  form = cdr(form);
                }
                return ret;
                
              }
              cases = cdr(cases);
            }
          }
          return nil;

        case LISP_AND:
          {
            var cases = cdr(value);
            lisp_value result = t;
            while(is_nil(cases) == false){
              var c = car(cases);
              result = lisp_eval2(scope, c);
              if(is_nil(result)) return nil;
              cases = cdr(cases);
            }
            return result;
          }
        case LISP_OR:
          {
            var cases = cdr(value);
            lisp_value result = nil;
            while(is_nil(cases) == false){
              var c = car(cases);
              result = lisp_eval2(scope, c);
              if(is_nil(result) == false) return result;
              cases = cdr(cases);
            }
            return nil;
          }
        case LISP_GET_SCOPE:
          {
				return (lisp_value){.scope = lisp_scope_unstack(scope), .type = LISP_SCOPE}; 
          }
        }
      }
      size_t argcnt = lisp_optimize_statement(scope, cdr(value));
      
      if(lisp_is_in_error())
        return nil;
      
      if(is_function_native(first_value)){
        var n = first_value.nfunction;
        if(n->fptr == NULL){
          raise_string("function is null");
          return nil;
        }
        lisp_value args[n->nargs == -1 ? argcnt : MAX(argcnt, n->nargs)];
        lisp_value arglist = cdr(value);
        for(size_t i = 0; i < argcnt; i++){
          args[i] = lisp_eval2(scope, car(arglist));
          arglist = cdr(arglist);
        }
        
        if(n->nargs != -1){
          for(int i = argcnt; i < n->nargs; i++){
            args[i] = nil;
          }
          if(argcnt > n->nargs){
            raise_string("Invalid number of arguments");
            return nil;
          }
        }

        union{
          lisp_value (* nvar)(lisp_value * values, int count);
          lisp_value (* n0)();
          lisp_value (* n1)(lisp_value);
          lisp_value (* n2)(lisp_value, lisp_value);
          lisp_value (* n3)(lisp_value, lisp_value, lisp_value);
          lisp_value (* n4)(lisp_value, lisp_value, lisp_value, lisp_value);
          lisp_value (* n5)(lisp_value, lisp_value, lisp_value, lisp_value, lisp_value);
          lisp_value (* n6)(lisp_value, lisp_value, lisp_value, lisp_value, lisp_value, lisp_value);
			 
        }f;

        f.n0 = n->fptr;
        
        switch(n->nargs){
        case -1:
          return f.nvar(args, argcnt);
        case 0:
          return f.n0();
        case 1:
          return f.n1(args[0]);
        case 2:
          return f.n2(args[0], args[1]);
        case 3:
          return f.n3(args[0], args[1], args[2]);       
        case 4:
          return f.n4(args[0], args[1], args[2], args[3]);
        case 5:
          return f.n5(args[0], args[1], args[2], args[3], args[4]);
          
        case 6:
          return f.n6(args[0], args[1], args[2], args[3], args[4], args[5]);
        default:
          raise_string("Unsupported number of args");
        }
      }else if(is_function(first_value)){
        cons args0[argcnt];
         
        lisp_value things;
        {
          memset(args0, 0, sizeof(args0[0]) * argcnt);
          // evaluate the arguments
          things = lisp_sub_eval(scope, cdr(value), args0);
        }
        
        var f = first_value.function;
        cons args3[argcnt];
        memset(args3, 0, sizeof(args3[0]) * (argcnt));
        
        lisp_scope function_scope[1];
        
        lisp_scope_stack(function_scope, f->closure,  args3, argcnt);
        lisp_push_scope(function_scope);
		  
        var args = f->args;
        var args2 = things;

        while(!is_nil(args)){
          var arg = car(args);
          if(!is_symbol(arg)){
            println(f->args);
            raise_string("(3) arg name must be a symbol");
            return nil;
          }
          if(lisp_value_eq(arg, rest_sym)){
            args = cdr(args);
            arg = car(args);
            if(!is_symbol(arg)){
              // error
              println(arg);
              raise_string("(4) arg name must be a symbol");
              return nil;
            }
            lisp_scope_create_value(function_scope, arg, copy_cons(args2));
            break;
          }
          var argv = car(args2);
          lisp_scope_create_value(function_scope, arg, argv);
			 
          args = cdr(args);
          args2 = cdr(args2);
        }

        var it = f->code;
        lisp_value ret = nil;
        while(!is_nil(it)){
          ret = lisp_eval(function_scope, car(it));
          it = cdr(it);
        }
        lisp_pop_scope(function_scope);
        return ret;
      }else{
        raise_string("not a function");
        return nil;
      }
    }
    break;
  case LISP_SYMBOL:
    {
      if(is_keyword(value))
        return value;
      lisp_value r;
      if(lisp_scope_try_get_value(scope, value, &r))
        return r;
      else{
        print(value);
        raise_string(" symbol not found.");
      }
    }
          
  case LISP_GLOBAL_INDEX:
    return current_context->globals->values[value.integer];
  case LISP_LOCAL_INDEX:
    while(value.local_index.scope_level > 0){
      scope = scope->super;
      ASSERT(scope != NULL);
      value.local_index.scope_level -= 1;
    }
    if(value.local_index.scope_type == 0){
      cons v = scope->lookup[value.local_index.scope_index];
      return v.cdr;
    }else{
      return scope->values[value.local_index.scope_index];
    }
        
  default:
    return value;
  }
  return nil;
}

lisp_value lisp_eval_value(lisp_value code, lisp_value scope){
  lisp_scope * scopeptr;
  if(!is_nil(scope)){
    TYPE_ASSERT(scope, LISP_SCOPE);
    scopeptr = scope.scope;
  }else{
    scopeptr = current_context->globals;
  }
  while(true){
    var next_code = lisp_macro_expand(scopeptr, code);
		
    if(lisp_value_eq(next_code, code))
      break;
    code = next_code;
  }
  
  return lisp_eval(scopeptr, code);
}

lisp_value lisp_eval_stream(io_reader * rd){
  lisp_value result = nil;
  while(true){
    gc_collect_garbage(current_context);
    var off = rd->offset;
    var code = lisp_read_stream(rd);
    if(off == rd->offset || is_nil(code)) break;
    while(true){
      if(lisp_is_in_error())
        return nil;
      lisp_register_value("lisp:++current-toplevel++", code);
      var next_code = lisp_macro_expand(current_context->globals, code);
		
      if(lisp_value_eq(next_code, code))
        break;
      code = next_code;
    }
    println(code);
    lisp_register_value("lisp:++current-toplevel++", code);
    result = lisp_eval(current_context->globals, code);
    if(lisp_is_in_error())
      return nil;
  }
  return result;
}

lisp_value lisp_eval_string(const char * str){
  io_reader w = io_reader_from_bytes((void *)str, strlen(str) + 1);
  w.offset = 0;
  return lisp_eval_stream(&w);
}

lisp_value lisp_eval_file(const char * filepath){
  char * buffer = read_file_to_string(filepath);
  if(buffer == NULL) {
    printf("FILE: %s\n", filepath);
    raise_string("Unable to read file");
    return nil;
  }
  lisp_value r = lisp_eval_string(buffer);
  free(buffer);
  return r;
}

int print2(char * buffer, int l2, lisp_value v){
  char * initbuf = buffer;
  int l = 0;
#define OBUF buffer ? (initbuf + l) : buffer
#define LEN1 l2 ? (l2 - l) : 0

  switch(lisp_value_type(v)){
  case LISP_NIL:
    return snprintf(buffer, LEN1, "()");
  case LISP_T:
    return snprintf(buffer, LEN1, "t");
  case LISP_INTEGER:
  case LISP_INTEGER32:
    return snprintf(buffer, LEN1, "%lli", v.integer);
  case LISP_BYTE:
    return snprintf(buffer, LEN1, "%i", (u8)v.integer);
  case LISP_VECTOR:
    {
      int l = 0;
      l = snprintf(buffer, LEN1, "#(");
      
      var vector = lisp_value_vector(v);
      for(size_t i = 0; i < vector->count; i++){
        var elem = vector_ref(v, integer(i));
        if(i != 0) l += snprintf(OBUF, LEN1, " ");
        l += print2(OBUF, LEN1, elem);
      }
      l += snprintf(OBUF, LEN1, ")");
      return l;
    }
  case LISP_NATIVE_POINTER:
    if(v.integer == 0)
      return snprintf(buffer, LEN1, "NULL");
    else
      return snprintf(buffer, LEN1, "%p", v.integer);
  case LISP_FLOAT32:
  case LISP_RATIONAL:
    return snprintf(buffer, LEN1, "%f", v.rational);
  case LISP_STRING:
    return snprintf(buffer, LEN1, "\"%s\"", v.string);
  case LISP_SYMBOL:
    return snprintf(buffer, LEN1, "%s", symbol_name(v.integer));
  case LISP_FUNCTION:
    return snprintf(buffer, LEN1, "FUNCTION");
  case LISP_FUNCTION_MACRO:
    return snprintf(buffer, LEN1, "FUNCTION_MACRO");
  case LISP_FUNCTION_NATIVE:
    return snprintf(buffer, LEN1, "Native function");
  case LISP_MACRO_BUILTIN:
    return snprintf(buffer, LEN1, "MacroBuiltin");
  case LISP_HASHTABLE:
    return snprintf(buffer, LEN1, "HashTable");
  case LISP_SCOPE:
    return snprintf(buffer, LEN1, "Scope(%i)", v.scope->argcnt);
  case LISP_GLOBAL_INDEX:
    return snprintf(buffer, LEN1, "GLOBAL Index (%i)", v.integer);
  case LISP_LOCAL_INDEX:
    return snprintf(buffer, LEN1, "Local Index (%i,%i)",v.local_index.scope_level, v.local_index.scope_index);
case LISP_CONS:
    {
      int l = 0;
      l = snprintf(buffer, LEN1, "(");
      var first = true;
      while(is_cons(v)){
        if(first){
          first = false;
        }else{
          l += snprintf(OBUF, LEN1, " ");
        }
        l += print2(OBUF, LEN1, v.cons->car);
        v = v.cons->cdr;
      }
      if(!is_nil(v)){
        l += snprintf(OBUF, LEN1, " . ");
        l += print2(OBUF, LEN1, v);
      }
      l += snprintf(OBUF, LEN1, ")");
      return l;
    }
  }
  return 0;
}

lisp_value print(lisp_value v){
  char buffer[100];
  int l = print2(NULL,0,  v);
  char * str = l >= 95 ? malloc(l + 1) : buffer;
  print2(str, l+ 1, v);
  printf("%s", str);
  if(l >= 95)
    free(str);
  return integer(l);
}

lisp_value value_to_string(lisp_value v){
  int l = print2(NULL,0,  v);
  char * str = lisp_malloc(l+ 1);
  print2(str, l + 1, v);
  return string_lisp_value(str);
}

void lisp_register_value(const char * name, lisp_value value){
  lisp_scope_create_value(current_context->globals, get_symbol(name), value);
}
void lisp_register_native(const char * name, int nargs, void * fptr){
  native_function *nf= lisp_malloc(sizeof(*nf));
  nf->nargs = nargs;
  nf->fptr = fptr;
  lisp_value v = {
    .type = LISP_FUNCTION_NATIVE,
    .nfunction = nf
  };
  lisp_register_value(name, v);
}

void lisp_register_macro(const char * name, lisp_builtin builtin){
  lisp_value v = {
    .type = LISP_MACRO_BUILTIN,
    .builtin = builtin
  };
  
  lisp_register_value(name, v);
}

static void normalize_numericals(lisp_value * values, int count){
  static u8 numeric_tower[LISP_SCOPE];
  if(numeric_tower[0] == 0){
    numeric_tower[LISP_NIL] = 0xFF;
    numeric_tower[LISP_T] = 0x0;
    numeric_tower[LISP_CONS] = 0xFF;
    numeric_tower[LISP_BYTE] = 1;
    numeric_tower[LISP_INTEGER32] = numeric_tower[LISP_BYTE] + 1;
    numeric_tower[LISP_INTEGER] = numeric_tower[LISP_INTEGER32] + 1;
    numeric_tower[LISP_FLOAT32] = numeric_tower[LISP_INTEGER] + 1;
    numeric_tower[LISP_RATIONAL] = numeric_tower[LISP_FLOAT32] + 1;
  }
  ASSERT(count > 0);
  lisp_type type = values[0].type;
  for(int i = 1; i < count; i++){
    lisp_type thistype = LISP_NIL;
    if(is_integer(values[i])){
      if(values[i].integer <= 0xFF){
        thistype = LISP_BYTE;
      }else if (values[i].integer <= 0xFFFFFFFFL){
        thistype = LISP_INTEGER32;
      }else
        thistype = LISP_INTEGER;
    } 
    else if(values[i].type == LISP_FLOAT32){
      thistype = LISP_FLOAT32;
    }if(values[i].type == LISP_RATIONAL){
      thistype = LISP_RATIONAL;
    }
    if(numeric_tower[thistype] > numeric_tower[type])
      type = thistype;
  }
  if(is_float_type(type)){
    for(int i = 0; i < count; i++){
      if(is_integer_type(values[i].type))
        values[i].rational = values[i].integer;
      values[i].type = type;
    }
  }else{
    for(int i = 0; i < count; i++){
      if(is_float_type(values[i].type))
        values[i].integer = (int64_t)values[i].rational;
      values[i].type = type;
    }
  }
}

#define lisp_procn2(values, count, ff, lf) \
    if(count == 2 && values[0].type == values[1].type){  \
      if(is_float_type(values[0].type)){                 \
        double sum = values[0].rational;                 \
        sum = ff(sum, values[1].rational);               \
        return rational(sum);                            \
      }else{                                             \
        i64 sum = values[0].integer;                     \
        sum = lf(sum, values[1].integer);                \
        return integer(sum);                             \
      }                                                  \
    }                                                    \
    normalize_numericals(values, count);                 \
                                                         \
    if(is_float(values[0])){                             \
      double sum = values[0].rational;                   \
      for(int i = 1; i < count; i++)                     \
        sum = ff(sum, values[i].rational);                 \
      return rational(sum);                              \
    }else{                                               \
      i64 sum = values[0].integer;                       \
      for(int i = 1; i < count; i++)                     \
        sum = lf(sum, values[i].integer);                  \
      return integer(sum);                               \
    }                                                    \


static inline lisp_value lisp_procn(lisp_value * values, int count, void (*ff )(f64 * l, f64 * r), void (*lf )(i64 * l, i64 * r)){
  if(count == 2 && values[0].type == values[1].type){
    if(is_float_type(values[0].type)){
      double sum = values[0].rational;
      ff(&sum, &(values[1]).rational);
      return rational(sum);
    }else{
      i64 sum = values[0].integer;
      lf(&sum, &(values[1]).integer);
      return integer(sum);
    }
  }
  normalize_numericals(values, count);
  
  if(is_float(values[0])){
    double sum = values[0].rational;
    for(int i = 1; i < count; i++)
      ff(&sum, &(values[i]).rational);
    return rational(sum);
  }else{
    i64 sum = values[0].integer;
    for(int i = 1; i < count; i++)
      lf(&sum, &(values[i]).integer);
    return integer(sum);
  }
}


static inline lisp_value lisp_binn(lisp_value * values, int count, bool (*cmpf )(f64 l, f64 r), bool (*cmpi )(i64 l, i64 r)){
  if(count == 2 && values[0].type == values[1].type){
    if(is_float_type(values[0].type)){
      return cmpf(values[0].rational, values[1].rational) ? t : nil;
    }else{
      return cmpi(values[0].integer, values[1].integer) ? t : nil;
    }
  }
  normalize_numericals(values, count);
  if(is_float(values[0])){
    for(int i = 1; i < count; i++)
      if(!cmpf((values[i - 1]).rational, (values[i]).rational))
        return nil;
  }else{
    for(int i = 1; i < count; i++)
      if(!cmpi((values[i - 1]).integer, (values[i]).integer))
        return nil;
  }
  return t;
}

static inline f64 addf(f64 a, f64  b){
  return a + b;
}
static inline i64 addi(i64 a, i64 b){
  return a + b;
}
lisp_value lisp_addn(lisp_value * values, int count){
  lisp_procn2(values, count, addf, addi);
}

static void subf(f64 * a, f64 * b){
  *a -= *b;
}
static void subi(i64 * a, i64 * b){
  *a -= *b;
}
lisp_value lisp_subn(lisp_value * values, int count){
  return lisp_procn(values, count, subf, subi);
}

static void mulf(f64 * a, f64 * b){
  *a *= *b;
}
static void muli(i64 * a, i64 * b){
  *a *= *b;
}
lisp_value lisp_muln(lisp_value * values, int count){
  return lisp_procn(values, count, mulf, muli);
}

static void divf(f64 * a, f64 * b){
  *a /= *b;
}

static void divi(i64 * a, i64 * b){
  *a /= *b;
}

lisp_value lisp_divn(lisp_value * values, int count){
  return lisp_procn(values, count, divf, divi);
}

static void minimumf(f64 * a, f64 * b){
  *a = MIN(*b, *a);
}

static void minimumi(i64 * a, i64 * b){
  *a = MIN(*b, *a);
}

lisp_value lisp_minimumn(lisp_value * values, int count){
  return lisp_procn(values, count, minimumf, minimumi);
}


static void maximumf(f64 * a, f64 * b){
  *a = MAX(*b, *a);
}

static void maximumi(i64 * a, i64 * b){
  *a = MAX(*b, *a);
}

lisp_value lisp_maximumn(lisp_value * values, int count){
  return lisp_procn(values, count, maximumf, maximumi);
}


static bool lessf(f64 a, f64 b){
  return a < b;
}
static bool lessi(i64 a, i64 b){
  return a < b;
}

lisp_value lisp_lessn(lisp_value * values, int count){
  return lisp_binn(values, count, lessf, lessi);
}

static bool lesseqf(f64 a, f64 b){
  return a <= b;
}
static bool lesseqi(i64 a, i64 b){
  return a <= b;
}

lisp_value lisp_lesseqn(lisp_value * values, int count){
  return lisp_binn(values, count, lesseqf, lesseqi);
}

static bool greaterf(f64 a, f64 b){
  return a > b;
}
static bool greateri(i64 a, i64 b){
  return a > b;
}

lisp_value lisp_greatern(lisp_value * values, int count){
  return lisp_binn(values, count, greaterf, greateri);
}

static bool greatereqf(f64 a, f64 b){
  return a >= b;
}

static bool greatereqi(i64 a, i64 b){
  return a >= b;
}

lisp_value lisp_greatereqn(lisp_value * values, int count){
  return lisp_binn(values, count, greatereqf, greatereqi);
}


lisp_value lisp_value_eqn(lisp_value * values, int count){
  for(int i = 1; i < count; i++)
    if(!lisp_value_eq(values[i-1], values[i]))
      return nil;
  return t;
}

lisp_value lisp_neqn(lisp_value * values, int count){
  for(int i = 0; i < count; i++)
    for(int j = i + 1; j < count; j++)
      if(lisp_value_eq(values[i], values[j]))
        return nil;
  return t;
}

bool eq(lisp_value a, lisp_value b){
  return lisp_value_eq(a, b);
}

lisp_value lisp_len(lisp_value a){
  int64_t i = 0;
  while(is_cons(a)){
    i += 1;
    a = cdr(a);
  }
  return integer_lisp_value(i);
}

lisp_value println(lisp_value v){
  print(v);
  printf("\n");
  return v;
}

lisp_value lisp_integer(lisp_value v){
  return integer_lisp_value(lisp_value_as_integer(v));
}

lisp_value lisp_byte(lisp_value v){
  return byte_lisp_value(lisp_value_as_integer(v));
}


lisp_value lisp_rational(lisp_value v){
  return rational_lisp_value(lisp_value_as_rational(v));
}

lisp_value rational(double v){
  return rational_lisp_value(v);
}

double as_rational(lisp_value v){
  return lisp_value_as_rational(v);
}

lisp_value float32(float v){
  return float32_lisp_value(v);;
}

lisp_value lisp_float32(lisp_value v){
  return float32_lisp_value(lisp_value_as_rational(v));
}

lisp_value lisp_panic(lisp_value v){
  return lisp_error(v);
}

lisp_value integer(int64_t v){
  return integer_lisp_value(v);
}

lisp_value byte(unsigned char v){
  return byte_lisp_value(v);
}

lisp_value native_pointer(void * ptr){
  return native_pointer_lisp_value(ptr);
}

lisp_value lisp_sin(lisp_value v){
  return rational_lisp_value(sin(lisp_value_as_rational(v)));
}

lisp_value lisp_read(lisp_value v){
  if(!is_string(v))
    return v;
  return lisp_read_string(lisp_value_string(v));
}

const char * lisp_type_to_string(lisp_type t){
  switch(t){
  case LISP_NIL: return "NIL";
  case LISP_T: return "T";
  case LISP_CONS: return "CONS";
  case LISP_INTEGER: return "INTEGER";
  case LISP_INTEGER32: return "INTEGER32";
  case LISP_RATIONAL: return "RATIONAL";
  case LISP_STRING: return "STRING";
  case LISP_SYMBOL: return "SYMBOL";
  case LISP_FUNCTION: return "FUNCTION";
  case LISP_FUNCTION_MACRO: return "MACRO";
  case LISP_FUNCTION_NATIVE: return "NATIVE_FUNCTION";
  case LISP_MACRO_BUILTIN: return "MACRO_BUILTIN";
  case LISP_NATIVE_POINTER: return "NATIVE_POINTER";
  case LISP_VECTOR: return "VECTOR";
  case LISP_BYTE: return "BYTE";
  case LISP_FLOAT32: return "FLOAT32";
  case LISP_HASHTABLE: return "HASHTABLE";
  case LISP_SCOPE: return "SCOPE";
  case LISP_GLOBAL_INDEX: return "GLOBAL_INDEX";
  case LISP_LOCAL_INDEX: return "LOCAL_INDEX";
  }
  raise_string("Unknown type:\n");
  
  return NULL;
}

lisp_value lisp_type_of(lisp_value v){
  return get_symbol(lisp_type_to_string(lisp_value_type(v)));
}

lisp_value lisp_load(lisp_value v){
  TYPE_ASSERT(v, LISP_STRING);
  return lisp_eval_file(lisp_value_string(v));
}

size_t lisp_type_size(lisp_type type){
  switch(type){
  case LISP_NIL: return sizeof(lisp_value);
  case LISP_BYTE: return sizeof(u8);
  case LISP_FLOAT32: return sizeof(float);
  default:break;
  }
  return sizeof(u64);
}

lisp_value make_vector(lisp_value len, lisp_value _default){
  TYPE_ASSERT(len, LISP_INTEGER);
  size_t l = lisp_value_integer(len);
  size_t elem_size = lisp_type_size(_default.type);
  void * data = lisp_malloc(l * elem_size);

  lisp_vector * vector = lisp_malloc(sizeof(*vector));
  vector->data = data;
  vector->count = l;
  vector->elem_size = elem_size;
  vector->default_value = _default;
  lisp_value v = vector_lisp_value(vector);
  for(size_t i = 0; i < l; i++)
    vector_set(v, integer_lisp_value(i),_default);
    
  return v;
}

lisp_value vector_length(lisp_value v){
  TYPE_ASSERT(v, LISP_VECTOR);
  return integer(lisp_value_vector(v)->count);
}

lisp_value vector_ref(lisp_value _vector, lisp_value k){
  TYPE_ASSERT(_vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var vector = lisp_value_vector(_vector);
  var i = lisp_value_integer(k);
  var v = vector->default_value;
  void * src = vector->data + i * vector->elem_size;
  void * dst;
  if(is_nil(v)){
    return ((lisp_value *) vector->data)[i];
  }else if(is_float32(v)){
    v.rational = ((float *) vector->data)[i];
    return v;
  }else
    dst = &v.integer;
  memcpy(dst, src, vector->elem_size);
  
  return v;
}

lisp_value vector_set(lisp_value _vector, lisp_value k, lisp_value value){
  TYPE_ASSERT(_vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var i = lisp_value_integer(k);
  var vector = lisp_value_vector(_vector);
  var v = value;
  void * dst = vector->data + i * vector->elem_size;
  if(i >= vector->count){
    raise_string("index outside of the bounds of the vector\n");
    return nil;
  }
  void * src;
  if(is_nil(vector->default_value))
    ((lisp_value *) vector->data)[i] = value;
  else{
    TYPE_ASSERT(value, vector->default_value.type);
    src = &v.integer;
    if(is_float32(vector->default_value)){
      ((float *) src)[0] = v.rational;
    }
	
    memcpy(dst, src, vector->elem_size);
  }
  return nil;
}

lisp_value vector_resize(lisp_value vector, lisp_value k){
  TYPE_ASSERT(vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);

  size_t l = (size_t)lisp_value_integer(k);
  var vec = lisp_value_vector(vector);
  size_t elem_size = lisp_type_size(lisp_value_type(vec->default_value));
  
  void * new_data = lisp_malloc(l * elem_size);
  size_t prevCount = MIN(l, vec->count);   
  memcpy(new_data, vec->data, prevCount * elem_size);
  vec->data = new_data;
  vec->count = l;
  for(size_t i = prevCount; i < l; i++){
    vector_set(vector, integer_lisp_value(i), vec->default_value);
  }

  return vector;
}

lisp_value vector_elem_type(lisp_value vector){
  TYPE_ASSERT(vector, LISP_VECTOR);
  return lisp_type_of(lisp_value_vector(vector)->default_value);
}

lisp_value vector_copy(lisp_value vector){
  TYPE_ASSERT(vector, LISP_VECTOR);
  var vec = lisp_value_vector(vector);
  let vector2 = make_vector(integer_lisp_value(vec->count), vec->default_value);
  var vec2 = lisp_value_vector(vector2);
  memcpy(vec2->data, vec->data, vec2->count * vec2->elem_size);
  return vector2;
}

lisp_value vector_to_string(lisp_value vector){
  vector = vector_copy(vector);
  var dst = lisp_value_vector(vector);
  dst->count = dst->count * dst->elem_size;
  dst->default_value = byte(0);
  var cnt = dst->count;
  u8 * ptr = dst->data;
  size_t i;
  for(i = 0; i < cnt; i++){
    if(ptr[i] == 0) break;
  }
  if(i == cnt)
    i = i + 1;
  vector = vector_resize(vector, integer_lisp_value(i));

  return string_lisp_value(dst->data);
}

lisp_value string_to_vector(lisp_value str){
  TYPE_ASSERT(str, LISP_STRING);
  char * strbuf = lisp_value_string(str);
  size_t l = strlen(strbuf) + 1;
  size_t elem_size = 1;

  lisp_vector * vector = lisp_malloc(sizeof(*vector));
  vector->data = strbuf;
  vector->count = l - 1;
  vector->elem_size = elem_size;
  vector->default_value = byte(0);
  return vector_lisp_value(vector);
}

lisp_value vector_native_element_pointer(lisp_value vector, lisp_value k){
  TYPE_ASSERT(vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var vec = lisp_value_vector(vector);
  void * ptr = vec->data + lisp_value_integer(k) * vec->elem_size;
  return native_pointer_lisp_value(ptr);
}

lisp_value parse_hex(lisp_value str){
  TYPE_ASSERT(str, LISP_STRING);
  char * tp = NULL;
  int64_t o = strtoll(str.string, &tp, 16);
  if(tp != lisp_value_string(str) )
    return integer_lisp_value(o); 
  return nil;
}

lisp_value hex_string(lisp_value i, lisp_value dec){
  var v = lisp_value_integer(i);
  var l = lisp_value_integer(dec);
  int cnt = snprintf(NULL, 0, "%x", v);
  if(is_nil(dec))
    l = cnt;
  char * buf = lisp_malloc(l + 1);
  snprintf(buf + MAX(0, l - cnt), MIN(cnt, l) + 1, "%x", v);
  buf[l] = 0;
  for(int i = 0; i < MAX(0, l - cnt); i++)
    buf[i] = '0';
  return string_lisp_value(buf);
}

lisp_value lisp_signature(lisp_value func){
  if(is_function(func))
    return new_cons(get_symbol("func"), func.function->args);
  return nil;
}


lisp_value lisp_make_hashtable(lisp_value weak_key, lisp_value weak_value){
  bool weak_keys = !is_nil(weak_key);
  bool weak_values = !is_nil(weak_value);
  hash_table * ht = ht_create(sizeof(lisp_value), sizeof(lisp_value));
  return hashtable_lisp_value(ht);
}

lisp_value lisp_hashtable_set(lisp_value _ht, lisp_value key, lisp_value value){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  ht_set(ht, &key, &value);
  return nil;
}

lisp_value lisp_hashtable_get(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  lisp_value value;
  if(ht_get(ht, &key, &value))
    return value;
  return nil;
}

void it_symbols(void * k1, void * k2, void * target){
  lisp_value * val = target;
  const int64_t * sym = k2;
  lisp_value prev = *val;
  *val = new_cons(symbol_lisp_value(*sym), prev); 
}

lisp_value lisp_all_symbols(){
  lisp_value val = nil;
  hash_table * symbols = current_context->symbols;
  ht_iterate(symbols, it_symbols, &val);
  return val;
}

lisp_value lisp_hashtable_remove(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  if(ht_remove(ht, &key))
    return t;
  return nil;
}

lisp_value lisp_hashtable_get2(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  lisp_value value;
  if(ht_get(ht, &key, &value))
    return new_cons(t, value);
  return nil;
}

lisp_value lisp_is_symbol(lisp_value v){
  if(is_symbol(v))
    return t;
  return nil;
}

lisp_value lisp_plookup(lisp_value lst, lisp_value sym){
  while(is_cons(lst)){
    if(is_symbol(car(lst))){
      if(lisp_value_eq(sym, car(lst)))
        return cadr(lst);
      lst = cddr(lst);
    }else{
      lst = cdr(lst);
    }

  }
  return nil;
}

lisp_value lisp_mapn(lisp_value f, lisp_value lst){
  var glob = current_context->globals;
  while(is_cons(lst)){
    var v = car(lst);
    cons e2 = {.car = v, .cdr = nil};
    lisp_value ev2 = cons_lisp_value(&e2);
    cons e1 = {.car = f, .cdr = ev2};
    lisp_value ev1 = cons_lisp_value(&e1);
    lisp_eval(glob, ev1);
    lst = cdr(lst);
  }
  return nil;
}
  

lisp_value lisp_exit(){
  exit(0);
  return nil;
}

lisp_value lisp_trace(lisp_value v){
  tracing = !is_nil(v);
  return nil;
}

void foxgl_register();
void lisp_process_module_init();
void load_modules(){
  foxgl_register();
  lisp_process_module_init();
}

void web_update(){
  var sym = get_symbol("lisp:*web-update*");
  lisp_eval(current_context->globals, new_cons(sym, nil));
}

int main(int argc, char ** argv){
  
  printf("starting..\n");
  current_context = lisp_context_new();
  
  lisp_register_native("lisp:count-allocated", 0, lisp_count_allocated);
  lisp_register_native("lisp:exit", 0, lisp_exit);
  lisp_register_native("lisp:trace", 1, lisp_trace);
  lisp_register_native("symbol?", 1, lisp_is_symbol);
  lisp_register_native("list?", 1, lisp_is_list);
  lisp_register_native("cons?", 1, lisp_is_cons);
  lisp_register_native("+", -1, lisp_addn);
  lisp_register_native("-", -1, lisp_subn);
  lisp_register_native("*", -1, lisp_muln);
  lisp_register_native("/",-1, lisp_divn);
  lisp_register_native("<", -1, lisp_lessn);
  lisp_register_native(">", -1, lisp_greatern);
  lisp_register_native("<=", -1, lisp_lesseqn);
  lisp_register_native(">=", -1, lisp_greatereqn);
  lisp_register_native("min", -1, lisp_minimumn);
  lisp_register_native("max", -1, lisp_maximumn);

  lisp_register_native("print", 1, print);
  lisp_register_native("println", 1, println);
  lisp_register_native("value->string", 1, value_to_string);
  lisp_register_native("car", 1, car);
  lisp_register_native("cdr", 1, cdr);
  lisp_register_native("cddr", 1, cddr);
  lisp_register_native("cadr", 1, cadr);
  lisp_register_native("set-car!", 2, set_car);
  lisp_register_native("set-cdr!", 2, set_cdr);
  lisp_register_native("cons", 2, new_cons);
  lisp_register_native("length", 1, lisp_length);
  lisp_register_native("lisp:collect-garbage", 0, lisp_collect_garbage);
  lisp_register_native("lisp:get-allocated", 0, lisp_get_allocated);
  lisp_register_native("lisp:trace-allocations", 1, lisp_trace_allocations);
  lisp_register_native("copy-list", 1, copy_cons);
  lisp_register_native("copy-list-deep", 1, copy_cons_deep);
  lisp_register_native("plookup", 2, lisp_plookup);
  //lisp_register_native("map!", 2, lisp_mapn);

  lisp_register_native("=", -1, lisp_value_eqn);
  lisp_register_native("/=", -1, lisp_neqn);
  lisp_register_native("string=", 2, string_eq);
  lisp_register_native("panic", 1, lisp_error);
  lisp_register_native("integer", 1, lisp_integer);
  lisp_register_native("rational", 1, lisp_rational);
  lisp_register_native("float32", 1, lisp_float32);
  lisp_register_native("byte", 1, lisp_byte);
  lisp_register_native("sin", 1, lisp_sin);
  lisp_register_native("type-of", 1, lisp_type_of);
  
  lisp_register_native("read-string", 1, lisp_read);
  lisp_register_native("load", 1, lisp_load);
  
  lisp_register_native("make-vector", 2, make_vector);
  lisp_register_native("vector-length", 1, vector_length);
  lisp_register_native("vector-ref", 2, vector_ref);
  lisp_register_native("vector-set!", 3, vector_set);
  lisp_register_native("vector-element-type", 1, vector_elem_type);
  lisp_register_native("vector-length", 1, vector_length);
  lisp_register_native("vector-native-element-pointer", 2, vector_native_element_pointer);
  lisp_register_native("vector-resize", 2, vector_resize);
  lisp_register_native("vector->string", 1, vector_to_string);
  lisp_register_native("string->vector", 1, string_to_vector);
  lisp_register_native("string-starts-with", 2, string_starts_with);
  lisp_register_native("parse-hex", 1, parse_hex);
  lisp_register_native("hex-string", 2, hex_string);

  lisp_register_native("symbol->string", 1, symbol_to_string);
  lisp_register_native("string->symbol", 1, string_to_symbol);
  lisp_register_native("all-symbols", 0, lisp_all_symbols);

  lisp_register_native("make-hashtable", 2, lisp_make_hashtable);
  lisp_register_native("hashtable-ref", 2, lisp_hashtable_get);
  lisp_register_native("hashtable-set", 3, lisp_hashtable_set);
  lisp_register_native("hashtable-remove", 2, lisp_hashtable_remove);
  lisp_register_native("hashtable-ref2", 2, lisp_hashtable_get2);
  lisp_register_native("function-signature", 1, lisp_signature);
  
  
  lisp_register_native("eval", 2, lisp_eval_value);
  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  lisp_register_macro("let", LISP_LET);
  lisp_register_macro("progn", LISP_PROGN);
  lisp_register_macro("lambda", LISP_LAMBDA);
  lisp_register_macro("loop", LISP_LOOP);
  lisp_register_macro("macro", LISP_MACRO);
  lisp_register_macro("set", LISP_SET);
  lisp_register_macro("def", LISP_DEFINE);
  lisp_register_macro("quasiquote", LISP_QUASIQUOTE);
  lisp_register_macro("case", LISP_CASE);
  lisp_register_macro("cond", LISP_COND);
  lisp_register_macro("and", LISP_AND);
  lisp_register_macro("or", LISP_OR);
  lisp_register_macro("unquote", LISP_UNQUOTE);
  lisp_register_macro("symbol-value", LISP_SYMBOL_VALUE);
  lisp_register_macro("bound?", LISP_BOUND);
  lisp_register_macro("with-exception-handler", LISP_WITH_EXCEPTION_HANDLER);
  lisp_register_macro("lisp:get-current-scope", LISP_GET_SCOPE);

  lisp_register_value("native-null-pointer", (lisp_value){.type = LISP_NATIVE_POINTER, .integer = 0});
#ifndef WASM
  lisp_register_value("lisp:*web-environment*", nil);
#else
  lisp_register_value("lisp:*web-environment*", t);
  lisp_register_value("lisp:*web-update*", nil);
#endif

  load_modules();
  //lisp_register_value("lisp:*root-scope*", nil);
#ifndef WASM
  lisp_register_value("lisp:*test-enabled*", nil);
  for(int i = 1; i < argc; i++){
    if(strcmp(argv[i], "--test") == 0){
      lisp_register_value("lisp:*test-enabled*", t);
    }else
      lisp_eval_file(argv[i]);
  }
#else
  lisp_eval_file("ld50.lisp");
#endif
  printf("Finished..\n");
#ifdef WASM
  emscripten_set_main_loop(web_update, 0, 1);
#endif
  
  return 0;
}

#ifdef WASM
EMSCRIPTEN_KEEPALIVE
void * lisp_read_file(const char * filename){
  printf("read file: %s\n", filename);
  return file_to_string(filename);
}
EMSCRIPTEN_KEEPALIVE
void lisp_invoke_string(const char * code){
  lisp_eval_string(code);
}

#endif

#include<pthread.h>
pthread_t foxlisp_create_thread(void * (* f)(void * data), void * data){
  pthread_t thread = {0};
  pthread_create(&thread, NULL, f, data);
  return thread;
}
