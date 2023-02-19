
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>
#include <setjmp.h>
#ifdef EMSCRIPTEN
#include "emscripten.h"
#endif

#include "foxlisp.h"

#include "foxlisp_internal.h"
#include "lisp_value2.c"

bool debug_enabled = false;

bool tracing = false;

lisp_value rest_sym = {.type = LISP_SYMBOL};
lisp_value if_sym = {.type = LISP_SYMBOL};
lisp_value quote_sym = {.type = LISP_SYMBOL};
lisp_value quasiquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_splice_sym = {.type = LISP_SYMBOL};
lisp_value fpe_sym = {.type = LISP_SYMBOL};
lisp_value current_error_sym = {.type = LISP_SYMBOL};
lisp_value nil = {0};
lisp_value t = {.type = LISP_T};
lisp_value else_sym;
lisp_value otherwise_sym;

// debug info
lisp_value read_current_file = {0};
lisp_value read_cons_offset = {0};
lisp_value read_cons_file = {0};

#undef ASSERT
void print_call_stack();
static lisp_value current_error = {0};
static lisp_value current_error_stack = {0};

static inline bool lisp_is_in_error(){
  return !is_nil(current_error);
}

bool lisp_error_state(){
  return !is_nil(current_error);
}

void raise_string(const char * str){
  current_error = string_lisp_value(str);
  current_error_stack = copy_cons(lisp_stack);
}


lisp_value lisp_error(lisp_value value){
  current_error = value;
  current_error_stack = copy_cons(lisp_stack);
  return nil;
}

bool type_assert(lisp_value val, lisp_type type){
  if(lisp_value_type(val) != type){
    char buffer[1000];
	 sprintf(buffer, "Invalid type, expected %s, but got %s\n",
            lisp_type_to_string(type),
            lisp_type_to_string(lisp_value_type(val)));
	 raise(SIGINT);
	 raise_string(nogc_clone(buffer, strlen(buffer) + 1));
    return false;
  }
  return true;
}

bool elem_type_assert(lisp_value vector, lisp_type type){
  return type_assert(lisp_value_vector(vector)->default_value, type);
}

#define ASSERT(x) if(!(x)){raise_string("!!! " #x "\n"); }


lisp_scope * lisp_scope_new(lisp_scope * super){
  lisp_scope * s = lisp_malloc(sizeof(*super));
  *s = (lisp_scope){0};
  s->super = super;
  return s;
}

void lisp_scope_stack(lisp_scope * s, lisp_scope * super, cons * lookup, size_t cnt){
  s->sub_scope = NULL;
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
  UNUSED(userdata);
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
  lisp_scope * newscope = gc_clone(scope, sizeof(newscope[0]));
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

lisp_value * lisp_scope_pointer(lisp_scope * scope, i64 sym){
  int index = 0;
 start:
  if(scope == NULL) return NULL;
  if(scope->lookup != NULL){
    var argcnt = scope->argcnt;
    for(size_t i = 0; i < argcnt; i++){
      var v = lisp_value_symbol(scope->lookup[i].car);
      if(v == 0) break;
      if(v == sym)
        return &scope->lookup[i].cdr;
    }
  }
  if(scope->values != NULL && ht_get(scope->values_index, &sym, &index)){
    return &scope->values[index];
  }
  scope = scope->super;
  goto start;
  return NULL;;
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
  lisp_symbol i = lisp_value_symbol(sym);
  int idx;
  
  if(scope->values == NULL){
	size_t scope_size = 8;
	if(scope->super == NULL){
	  scope_size = 2048;
	}
    scope->values_capacity = scope_size;
    scope->values_index = ht_create2(scope->values_capacity, sizeof(u64), sizeof(int));
    scope->values_index->hash = symbol_nohash;
    scope->values = alloc(scope->values_capacity * sizeof(lisp_value));
    scope->value_symbol = alloc(scope->values_capacity * sizeof(lisp_symbol));
    scope->values[0] = nil;
    scope->value_symbol[0] = 0;
    scope->values_count = 1;
  }else if(ht_get(scope->values_index, &i, &idx)){
    scope->values[idx] = value;
    return nil;
  }
  
  if(scope->values_count == scope->values_capacity){
	scope->values_capacity *= 2;
    scope->values = realloc(scope->values, scope->values_capacity * sizeof(lisp_value)); 
    scope->value_symbol = realloc(scope->value_symbol, scope->values_capacity * sizeof(lisp_symbol));
  }
  
  idx = scope->values_count;
  ht_set(scope->values_index, &i, &idx);
  scope->values[scope->values_count] = value;
  scope->value_symbol[scope->values_count] = i;
  scope->values_count += 1;
  return nil;
}

lisp_context * current_context;
lisp_scope * lisp_get_root_scope(){
  return current_context->globals;
}

lisp_value car_nocheck(lisp_value v){
  return lisp_value_cons(v)->car;
}

inline lisp_value car(lisp_value v){
  if(is_cons(v))
	 return car_nocheck(v);
  return nil;
}

lisp_value cdr_nocheck(lisp_value v){
  return lisp_value_cons(v)->cdr;
}

inline lisp_value cdr(lisp_value v){
  if(is_cons(v))
	 return cdr_nocheck(v);
  return nil;
}

inline lisp_value cddr(lisp_value v){
  return cdr(cdr(v));
}

inline lisp_value cadr(lisp_value v){
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
	 lst = cdr_nocheck(lst);
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

lisp_value get_symbol_cached(lisp_value * v, const char * s){
  if(!is_nil(*v))
    return *v;
  return *v = get_symbol(s);
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

lisp_value lisp_value_deref(lisp_value ptr){
  TYPE_ASSERT(ptr, LISP_NATIVE_POINTER_TO_VALUE);
  lisp_value * pt = ptr.pointer;
  if(pt == NULL) return nil;
  return *pt;
}


lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value);

// new cons for macros
// this makes sure that the code location is maintained.
lisp_value new_cons_m(lisp_value a, lisp_value b, lisp_value cons){
  
  if(is_nil(read_cons_offset))
    return new_cons(a, b);
  var offset = lisp_hashtable_get(read_cons_offset, cons);
  var file = lisp_hashtable_get(read_cons_file, cons);
  cons = new_cons(a, b);
  
  if(is_nil(offset) || is_nil(file))
     return cons;
  lisp_hashtable_set(read_cons_offset, cons, offset);
  lisp_hashtable_set(read_cons_file, cons, file);
  
  return cons;
}

lisp_value lisp_sub_macro_expand(lisp_scope * scope, lisp_value c){
  if(is_nil(c)) return nil;
  var current = car(c);
  var next = cdr(c);
  var value = lisp_macro_expand(scope, current);
  if(!is_nil(next)){
	var nextr = lisp_sub_macro_expand(scope, next);
	if(lisp_value_eq(current, value) && lisp_value_eq(next, nextr))
	  return c;
	return new_cons_m(value, nextr, c);
  }else{
	if(lisp_value_eq(current, value))
	  return c;
	return new_cons_m(value, nil, c);
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
  var prev_scope = scope->sub_scope; 
  scope->sub_scope = function_scope;
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
                scope->sub_scope = prev_scope;
		return nil;
	 }
	 if(lisp_value_eq(arg, rest_sym)){
		args = cdr(args);
		arg = car(args);
		if(!is_symbol(arg)){
		  raise_string("(2) arg name must be a symbol.");
                  scope->sub_scope = prev_scope;
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
  scope->sub_scope = prev_scope;
  
  return ret;
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

lisp_value lisp_collect_garbage(){
  gc_collect_garbage(current_context);
  return nil;
}

static inline size_t lisp_optimize_statement(lisp_scope * scope, lisp_value statement){
  size_t c = 0;
  while(is_cons(statement)){
    var first = car_nocheck(statement);
    switch(lisp_value_type(first)){
    case LISP_SYMBOL:
      {
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
      break;
      }
    case LISP_GLOBAL_INDEX:
    case LISP_LOCAL_INDEX:
      return c + 1 + list_length(cdr_nocheck(statement));
    default:
      break;
    }
    statement = cdr(statement);
    c += 1;
  }
  return c;
}

lisp_value _lisp_optimize_statement(lisp_value statement, lisp_value _scope){
  lisp_scope * scope = NULL;
  if(is_scope(_scope) == false){
    scope = _scope.scope;
  }else{
    scope = current_context->globals;
  }
  lisp_optimize_statement(scope, statement);
  return nil;
}

lisp_value lisp_function_code(lisp_value function){
  TYPE_ASSERT(function, LISP_FUNCTION);
  var f = lisp_value_function(function);
  return f->code;
}

lisp_value lisp_scope_vars(lisp_value _scope){
  TYPE_ASSERT(_scope, LISP_SCOPE);
  var values = nil;
  var scope = lisp_value_scope(_scope);
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(is_nil(scope->lookup[i].car))
        break;
	  values = new_cons(new_cons(scope->lookup[i].car, scope->lookup[i].cdr), values);
	}
  }
  return values;
}

// return the super scope.
lisp_value lisp_scope_super(lisp_value scope){
  TYPE_ASSERT(scope, LISP_SCOPE);
  lisp_scope * scope2 = lisp_value_scope(scope);
  if(scope2->super == NULL)
	return nil;
  return scope_lisp_value(scope2->super);
}

lisp_value lisp_sub_scope(lisp_value scope, lisp_value sym, lisp_value value){
  lisp_scope s;
  lisp_scope * super_scope = lisp_value_scope(scope);
  cons con = {.car = sym, .cdr = value};
  lisp_scope_stack(&s, super_scope, &con, 1);
  return scope_lisp_value(lisp_scope_unstack(&s));
}

lisp_value lisp_with_sub_scope(lisp_value scope, lisp_value sym, lisp_value value, lisp_value body){
  lisp_scope s;
  lisp_scope * super_scope = lisp_value_scope(scope);
  cons con = {.car = sym, .cdr = value};
  lisp_scope_stack(&s, super_scope, &con, 1);
  return lisp_eval_progn(&s, body);
}


lisp_value lisp_with_scope_binding(lisp_value scope, lisp_value scope2, lisp_value syms, lisp_value values, lisp_value body){
  
  lisp_scope s;
  lisp_scope * super_scope = lisp_value_scope(scope);
  lisp_scope * scope22 = is_nil(scope2) ? NULL : lisp_value_scope(scope2);
  size_t len = lisp_value_integer(lisp_length(values));
  size_t len2 = lisp_value_integer(lisp_length(syms));
  cons con[len + len2];
  lisp_value it = values;
  for(size_t i = 0; i < len; i++){
    lisp_value form = car(it);
    it = cdr(it);
    var sym = car(form);
    var body = cadr(form);
    con[i].car = sym;
    con[i].cdr = body;
  }
  it = syms;
  
  for(size_t i = 0; i < len2; i++){
    con[len + i].car = car(it);
    con[len + i].cdr = lisp_scope_get_value(scope22, con[len + i].car);
    it = cdr(it);
  }
  lisp_scope_stack(&s, super_scope, con, len + len2);
  
  for(size_t i = 0; i < len; i++){
    var code = con[i].cdr;
    con[i].cdr = lisp_eval(&s, code); 
    if(lisp_is_in_error()){
      lisp_error(new_cons(new_cons(string_lisp_value("scope binding"), code),  current_error));
      return nil;
    }
    
  }
  return lisp_eval_progn(&s, body);
}

lisp_value lisp_with_scope_vars(lisp_value scope, lisp_value scope2, lisp_value syms, lisp_value variable, lisp_value body){
  lisp_scope s;
  lisp_scope * super_scope = lisp_value_scope(scope);
  lisp_scope * scope22 = lisp_value_scope(scope2);
  size_t len2 = lisp_value_integer(lisp_length(syms));
  cons con[len2 + 1];
  lisp_value it = syms;
  
  for(size_t i = 0; i < len2; i++){
    con[i].car = car(it);
    con[i].cdr = lisp_scope_get_value(scope22, con[i].car);
    it = cdr(it);
  }
  con[len2].car = variable;
  con[len2].cdr = nil;
  
  lisp_scope_stack(&s, super_scope, con, len2 + 1);

  return lisp_eval_progn(&s, body);
}

lisp_value lisp_scope_set(lisp_value scope, lisp_value sym, lisp_value value){
  lisp_scope * s = lisp_value_scope(scope);
  lisp_scope_set_value(s, sym, value);
  return nil;
}

inline lisp_value lisp_eval_progn(lisp_scope * scope, lisp_value body){
  lisp_value result = nil;
  while(!is_nil(body)){
    result = lisp_eval(scope, car(body));
    if(lisp_is_in_error()) break;
    body = cdr(body);
  }
  return result;
}

lisp_value lisp_eval_if(lisp_scope * scope, lisp_value cond, lisp_value true_form, lisp_value false_form){
  var condr = lisp_eval(scope, cond);
  if(lisp_is_in_error()) return nil;
  var pin = lisp_pin(condr);
  lisp_value r;
  if(is_nil(condr))
    r = lisp_eval(scope, false_form);
  else
	r = lisp_eval(scope, true_form);
  lisp_unpin(pin);
  return r;
}

lisp_value lisp_eval_symbol(lisp_scope * scope, lisp_value sym){

  // a keyword is defined as a symbol, storing its own value.
  if(is_keyword(sym))
    return sym;
  
  lisp_value r = nil;
  if(!lisp_scope_try_get_value(scope, sym, &r))
    lisp_error(new_cons(string_lisp_value("Symbol not found"), sym));
  return r;
}


static inline lisp_value lisp_eval_let(lisp_scope * scope, lisp_value argform, lisp_value body){

  var argcnt = list_length(argform);
  cons argsbuf[argcnt];
  memset(argsbuf, 0, sizeof(argsbuf[0]) * argcnt);
  lisp_scope scope1[1] = {0};
  lisp_scope_stack(scope1, scope, argsbuf, argcnt);
  scope->sub_scope = scope1;
  var scope_old = scope;
  lisp_pin_args(argsbuf, argcnt);
  scope = scope1;
  while(!is_nil(argform)){
    
    var arg = car(argform);
    var sym = car(arg);
    var value = lisp_eval(scope, cadr(arg));
    if(lisp_is_in_error()){
      scope_old->sub_scope = NULL;
	  lisp_unpin_args(argsbuf, argcnt);
	  return nil;
    }
    lisp_scope_create_value(scope, sym, value);
    argform = cdr(argform);
  }
  lisp_value result = lisp_eval_progn(scope, body);
  lisp_unpin_args(argsbuf, argcnt);
  scope_old->sub_scope = NULL;
  return result;
}

lisp_value lisp_eval_loop(lisp_scope * scope, lisp_value condition, lisp_value body){
  lisp_value result = nil;
  while(!is_nil(lisp_eval(scope, condition))){
    result = lisp_eval_progn(scope, body);
    if(lisp_is_in_error()) break;
  }
  return result;
}

lisp_value lisp_eval_symbol_value(lisp_scope * scope, lisp_value sym_expr, lisp_value root_scope_only){
  var sym = lisp_eval(scope, sym_expr);
  lisp_value result = nil;
  TYPE_ASSERT(sym, LISP_SYMBOL);
  
  var root_scope_type = lisp_value_type(root_scope_only);
  //if looking for a symbol at root scope, iterate through the scopes. 
  if(root_scope_type != LISP_NIL){
    if(root_scope_type != LISP_T)
      root_scope_only = lisp_eval(scope, root_scope_only);
    
    if(lisp_value_type(root_scope_only) == LISP_SCOPE){
      scope = lisp_value_scope(root_scope_only);
    }else{
      while(scope->super != NULL)
        scope = scope->super;
    }
  }
  if(lisp_scope_try_get_value(scope, sym, &result))
    return result;
  return nil;
}


lisp_value lisp_eval_boundp(lisp_scope * scope, lisp_value sym_expr,
							lisp_value root_scope_only_expr){
  var target_scope = scope;
  if(!is_nil(root_scope_only_expr) && !is_nil(lisp_eval(scope, root_scope_only_expr)))
    target_scope = current_context->globals;
  
  var sym = lisp_eval(scope, sym_expr);
  if(!is_symbol(sym))
    return nil;
  lisp_value result = nil;
  if(lisp_scope_try_get_value(target_scope, sym, &result))
    return t;
  return nil;
}

lisp_value lisp_eval_with_exception_handler(lisp_scope * scope,
											lisp_value body, lisp_value handler){
  
  var stk0 = lisp_pin(lisp_stack);
  lisp_stack = nil;
  var result = lisp_eval(scope, body);
  if(lisp_is_in_error()){
	var error = current_error;
    current_error = nil;
    var error_handler = lisp_eval(scope, handler);
    lisp_scope_create_value(current_context->globals, current_error_sym, error);
    var result2 = lisp_eval(scope, new_cons(error_handler, new_cons(current_error_sym, nil)));
    current_error_stack = nil;
    
    result = result2;
  }
  lisp_stack = lisp_unpin(stk0);
  return result;
}

static lisp_value lisp_eval_case(lisp_scope * scope, lisp_value condition,
								 lisp_value cases){
  var result = lisp_eval(scope, condition);
  var result_pin = lisp_pin(result);
  var result_type = lisp_value_type(result);
  while(is_cons(cases)){
    var c = car_nocheck(cases);
    var test = car(c);
    var test_type = lisp_value_type(test);
    if(test_type == LISP_CONS){
      while(is_cons(test)){
        if(lisp_value_eq(result, car_nocheck(test))){
		  lisp_unpin(result_pin);
		  return lisp_eval_progn(scope, cdr(c));
		}
        
        test = cdr(test);
      }
    }else if (test_type == result_type && lisp_value_eq(result, test)){
	  lisp_unpin(result_pin);
	  return lisp_eval_progn(scope, cdr(c));
    }else if(test_type == LISP_SYMBOL && lisp_value_eq(test, otherwise_sym)){
	  lisp_unpin(result_pin);
      return lisp_eval_progn(scope, cdr(c));
    }
    
    cases = cdr_nocheck(cases);
  }
  lisp_unpin(result_pin);
  return nil;
}

lisp_value lisp_eval_cond(lisp_scope * scope, lisp_value cases){
  while(is_nil(cases) == false){
    var c = car(cases);
    var test = car(c);
    
    if(lisp_value_eq(test, else_sym) || !is_nil(lisp_eval(scope, test)))
      return lisp_eval_progn(scope, cdr(c));
    
    cases = cdr(cases);
  }
  return nil;
}

lisp_value lisp_eval_and(lisp_scope * scope, lisp_value cases){
  lisp_value result = t;
  while(is_nil(cases) == false){
    var c = car(cases);
    result = lisp_eval(scope, c);
    if(is_nil(result)) return nil;
    cases = cdr(cases);
  }
  return result;
}

lisp_value lisp_eval_or(lisp_scope * scope, lisp_value cases){
  while(is_nil(cases) == false){
    var c = car(cases);
    var result = lisp_eval(scope, c);
    if(is_nil(result) == false) return result;
    cases = cdr(cases);
  }
  return nil;
}

lisp_value lisp_eval_get_scope(lisp_scope * scope){
 return (lisp_value){.scope = lisp_scope_unstack(scope), .type = LISP_SCOPE}; 
}

lisp_value lisp_eval_ref(lisp_scope * scope, lisp_value sym){
  var t = lisp_value_type(sym);
  lisp_value * ptr;
  if(t == LISP_LOCAL_INDEX){
     while(sym.local_index.scope_level > 0){
      scope = scope->super;
      if(scope == NULL){
        raise_string("Invalid l-value optimization");
        return nil;
      }
      ASSERT(scope != NULL);
      sym.local_index.scope_level -= 1;
    }
    if(sym.local_index.scope_type == 0){
      cons * v = scope->lookup + sym.local_index.scope_index;
      ptr = &v->cdr;
    }else{
      lisp_value * v = scope->values + sym.local_index.scope_index;
      ptr = v;
    }
    
  } 
  else if(t == LISP_GLOBAL_INDEX){
    ptr = &current_context->globals->values[sym.integer];
  }else{
    TYPE_ASSERT(sym, LISP_SYMBOL);
    ptr = lisp_scope_pointer(scope, sym.integer);
  }
  if(ptr == NULL) return nil;
  lisp_value v = {.type = LISP_NATIVE_POINTER_TO_VALUE, .pointer = ptr};
  return v;
    
}


lisp_value lisp_eval_get_scope_unsafe(lisp_scope * scope){
  return (lisp_value){.scope = scope, .type = LISP_SCOPE};
}

lisp_value lisp_eval_lambda(lisp_scope * scope, lisp_value args, lisp_value body){
  lisp_function * f = lisp_malloc(sizeof(*f));
  f->code = body;
  f->args = args;
  scope = lisp_scope_unstack(scope);
  f->closure = scope;
  return (lisp_value){.type = LISP_FUNCTION, .function = f};
}

lisp_value lisp_eval_macro(lisp_scope * scope, lisp_value args, lisp_value body){
  var val = lisp_eval_lambda(scope, args, body);
  val.type = LISP_FUNCTION_MACRO;
  return val;
}

lisp_value lisp_eval_set(lisp_scope * scope, lisp_value symform){
  var sym = car(symform);
  var value2 = lisp_eval(scope, cadr(symform));
  switch(lisp_value_type(sym)){
  case LISP_SYMBOL:
    lisp_optimize_statement(scope, symform);
    lisp_scope_set_value(scope, sym, value2);
    break;
  case LISP_LOCAL_INDEX:
    while(sym.local_index.scope_level > 0){
      scope = scope->super;
      if(scope == NULL){
        raise_string("Invalid l-value optimization");
        return nil;
      }
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

lisp_value lisp_eval_define(lisp_scope * scope, lisp_value sym, lisp_value value){
  TYPE_ASSERT(sym, LISP_SYMBOL);
    
  if(!is_symbol(sym)){
    raise_string("Invalid symbol for define");
    return nil;
  }
  var value2 = lisp_eval(scope, value);
  if(lisp_is_in_error())
    return nil;
  lisp_scope_create_value(scope, sym, value2);
  return value2;
}

static sigjmp_buf error_resume_jmp;

lisp_value lisp_eval_native_functions(lisp_scope * scope, native_function * n, size_t argcnt, lisp_value arg_form){
  if(n->fptr == NULL){
    raise_string("function is null");
    return nil;
  }
  
  size_t argcnt2 = argcnt;
  if(n->nargs != -1)
	argcnt2 = MAX(argcnt2, (size_t) n->nargs);
  lisp_value args[argcnt2];
  lisp_array pin_array = {.array = args, .count = argcnt};
  void * pin = lisp_pin_array(&pin_array);
  lisp_value arglist = arg_form;
  for(size_t i = 0; i < argcnt; i++){
    args[i] = lisp_eval(scope, car(arglist));
    arglist = cdr(arglist);
    if(lisp_is_in_error()){
	  lisp_unpin(pin);
	  return nil;
	}
  }
        
  if(n->nargs != -1){
    for(int i = argcnt; i < n->nargs; i++){
      args[i] = nil;
    }
    if(argcnt > (size_t)n->nargs){
	  lisp_unpin(pin);
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
  
  int r111 = sigsetjmp(error_resume_jmp, 0);
  if(r111 == 1){
	lisp_unpin(pin);
    return nil;
  }
  gc_unsafe_stack = true;
  lisp_value r = nil;
  switch(n->nargs){
  case -1:
    r = f.nvar(args, argcnt);break;
  case 0:
    r = f.n0();break;
  case 1:
    r = f.n1(args[0]);break;
  case 2:
    r = f.n2(args[0], args[1]);break;
  case 3:
    r = f.n3(args[0], args[1], args[2]);break;       
  case 4:
    r = f.n4(args[0], args[1], args[2], args[3]);break;
  case 5:
    r = f.n5(args[0], args[1], args[2], args[3], args[4]);break;
  case 6:
    r = f.n6(args[0], args[1], args[2], args[3], args[4], args[5]);break;
  default:
    raise_string("Unsupported number of args");
  }
  gc_unsafe_stack = false;
  lisp_unpin(pin);
  return r;
}


t_cons_arrays cons_arrays = {0};

void lisp_pin_args(cons * argslist, size_t cnt){
  if(cons_arrays.count == cons_arrays.capacity){
	size_t new_capacity = cons_arrays.capacity * 2;
	if(new_capacity == 0)
	  new_capacity = 32;
	cons_arrays.arrays = realloc(cons_arrays.arrays, sizeof(cons_arrays.arrays[0]) * new_capacity);
	cons_arrays.capacity = new_capacity;	
  }
  if(cons_arrays.count > 2000){
	raise_string("cons array leak");
  }
  var new = &cons_arrays.arrays[cons_arrays.count];
  new->array = argslist;
  new->count = cnt;
  cons_arrays.count += 1;
}

void lisp_unpin_args(cons * argslist, size_t cnt){
  if(cons_arrays.count == 0){
	raise_string("cons array imbalance");
	return;
  }
  var top = &cons_arrays.arrays[cons_arrays.count - 1];
  if(top->array != argslist || top->count != cnt){
	raise_string("Unexpected event in lisp_unpin_args");
	return;
  }
  cons_arrays.count -= 1;

}

static inline lisp_value lisp_eval_function(lisp_scope * scope, lisp_function * f, size_t argcnt, lisp_value args_form){
  
  // these args needs to be gc_pinned 
  cons args0[argcnt];
  memset(args0, 0, sizeof(args0));
  lisp_pin_args(args0, argcnt);
  
  lisp_value things;
  {
    lisp_value arg0 = args_form;

    // evaluate the arguments
    for(size_t i = 0; i < argcnt; i++){
      args0[i].car = lisp_eval(scope, car(arg0));
      arg0 = cdr(arg0);
    }
          
    for(ssize_t i = 0; i < (ssize_t)(argcnt - 1); i++)
      args0[i].cdr = cons_lisp_value(args0 + i + 1);
          
    if(argcnt > 0){
      args0[argcnt -1].cdr = nil;
      things = cons_lisp_value(args0);
    }else
      things = nil;
  }

  
  cons args3[argcnt];
  memset(args3, 0, sizeof(args3[0]) * (argcnt));
        
  lisp_scope function_scope[1] = {0};
        
  lisp_scope_stack(function_scope, f->closure,  args3, argcnt);
  var prev_scope = scope->sub_scope;
  scope->sub_scope = function_scope;
        
  var args = f->args;
  var args2 = things;

  while(!is_nil(args)){
    var arg = car(args);
    if(!is_symbol(arg)){
      println(f->args);
      raise_string("(3) arg name must be a symbol");
      scope->sub_scope = prev_scope;
      return nil;
    }
    if(lisp_value_eq(arg, rest_sym)){
      args = cdr(args);
      arg = car(args);
      if(!is_symbol(arg)){
        // error
        println(arg);
        raise_string("(4) arg name must be a symbol");
        scope->sub_scope = prev_scope;
		lisp_unpin_args(args0, argcnt);

        return nil;
      }
      
      lisp_scope_create_value(function_scope, arg,  copy_cons(args2));
      break;
    }
    var argv = car(args2);
            
    lisp_scope_create_value(function_scope, arg, argv);
			 
    args = cdr(args);
    args2 = cdr(args2);
  }

  lisp_unpin_args(args0, argcnt);

  var it = f->code;
  lisp_value ret = nil;
  while(!is_nil(it)){
    ret = lisp_eval(function_scope, car(it));
    it = cdr(it);
  }
  scope->sub_scope = prev_scope;
  return ret;

}

bool eq(lisp_value a, lisp_value b){
  return lisp_value_eq(a, b);
}

lisp_value lisp_eval_eqn(lisp_scope * scope, lisp_value values){
  lisp_value a = lisp_eval(scope, car(values));
  values = cdr(values);
  while(!is_nil(values)){
    
    lisp_value b = lisp_eval(scope, car(values));
    values = cdr(values);
    if(!lisp_value_eq(a, b))
      return nil;
  }
  return t;
}


lisp_value lisp_eval_plookup(lisp_scope * scope, lisp_value lst, lisp_value sym){
  lst = lisp_eval(scope, lst);
  sym = lisp_eval(scope, sym);
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

lisp_value lisp_eval_add(lisp_scope * scope, lisp_value number_forms){
  lisp_value val1 = lisp_eval(scope, car(number_forms));
  lisp_type val1_type = lisp_value_type(val1);
  bool integer_mode = !is_float_type(val1_type);
  
  while(!is_nil(number_forms = cdr(number_forms))){
    lisp_value val2 = lisp_eval(scope, car(number_forms));
    lisp_type val2_type = lisp_value_type(val2);
    if(integer_mode){
      if(is_float(val2)){
        integer_mode = false;
        val1 = rational(val1.integer);
        val1_type = val2_type;
        goto float_mode;
      }else{
        val1.integer += val2.integer;
        if(val2_type != val1_type){
          if(val1_type == LISP_INTEGER)
            val1_type = LISP_INTEGER;
          else if(val2_type == LISP_INTEGER32 && val1_type == LISP_BYTE)
            val1_type = LISP_INTEGER;
        }
      }
    }else{
    float_mode:
      if(!is_float(val2)){
        val2.rational = val2.integer;
      }
      val1.rational += val2.rational;
      if(val2_type != val1_type && val2_type == LISP_RATIONAL){
        val1_type = LISP_RATIONAL;
      }
    }
  }
  val1.type = val1_type;
  return val1;
}
lisp_value lisp_eval_sub(lisp_scope * scope, lisp_value number_forms){
  lisp_value val1 = lisp_eval(scope, car(number_forms));
  lisp_type val1_type = lisp_value_type(val1);
  bool integer_mode = !is_float_type(val1_type);
  
  while(!is_nil(number_forms = cdr(number_forms))){
    lisp_value val2 = lisp_eval(scope, car(number_forms));
    lisp_type val2_type = lisp_value_type(val2);
    if(integer_mode){
      if(is_float(val2)){
        integer_mode = false;
        val1 = rational(val1.integer);
        val1_type = val2_type;
        goto float_mode;
      }else{
        val1.integer -= val2.integer;
        if(val2_type != val1_type){
          if(val1_type == LISP_INTEGER)
            val1_type = LISP_INTEGER;
          else if(val2_type == LISP_INTEGER32 && val1_type == LISP_BYTE)
            val1_type = LISP_INTEGER;
        }
      }
    }else{
    float_mode:
      if(!is_float(val2)){
        val2.rational = val2.integer;
      }
      val1.rational -= val2.rational;
      if(val2_type != val1_type && val2_type == LISP_RATIONAL){
        val1_type = LISP_RATIONAL;
      }
    }
  }
  val1.type = val1_type;
  return val1;
}
lisp_value lisp_eval_mul(lisp_scope * scope, lisp_value number_forms){
  lisp_value val1 = lisp_eval(scope, car(number_forms));
  lisp_type val1_type = lisp_value_type(val1);
  bool integer_mode = !is_float_type(val1_type);
  
  while(!is_nil(number_forms = cdr(number_forms))){
    lisp_value val2 = lisp_eval(scope, car(number_forms));
    lisp_type val2_type = lisp_value_type(val2);
    if(integer_mode){
      if(is_float(val2)){
        integer_mode = false;
        val1 = rational(val1.integer);
        val1_type = val2_type;
        goto float_mode;
      }else{
        val1.integer *= val2.integer;
        if(val2_type != val1_type){
          if(val1_type == LISP_INTEGER)
            val1_type = LISP_INTEGER;
          else if(val2_type == LISP_INTEGER32 && val1_type == LISP_BYTE)
            val1_type = LISP_INTEGER;
        }
      }
    }else{
    float_mode:
      if(!is_float(val2)){
        val2.rational = val2.integer;
      }
      val1.rational *= val2.rational;
      if(val2_type != val1_type && val2_type == LISP_RATIONAL){
        val1_type = LISP_RATIONAL;
      }
    }
  }
  val1.type = val1_type;
  return val1;
}

lisp_value lisp_eval_vector_length(lisp_scope * scope, lisp_value vec_form){
  return vector_length(lisp_eval(scope, vec_form));
}

lisp_value lisp_eval_vector_ref(lisp_scope * scope, lisp_value vec_form, lisp_value index_form){
  return vector_ref(lisp_eval(scope, vec_form), lisp_eval(scope, index_form));
}

lisp_value lisp_eval_vector_set(lisp_scope * scope, lisp_value vec_form, lisp_value index_form, lisp_value value_form){
  var vec = lisp_eval(scope, vec_form);
  if(is_nil(vec) && lisp_is_in_error())
	return nil;
  var index = lisp_eval(scope, index_form);
  if(is_nil(index) && lisp_is_in_error())
	return nil;
  var value = lisp_eval(scope, value_form);
  if(lisp_is_in_error())
	return nil;
  
  return vector_set(vec, index, value);
}

lisp_value lisp_stack;
int lisp_stack_size;
const int lisp_max_stack_size = 2000;
lisp_value lisp_eval_inner(lisp_scope * scope, lisp_value value);
lisp_value lisp_eval(lisp_scope * scope, lisp_value value){
  if(lisp_stack_size > lisp_max_stack_size){
	 raise_string("Max stack size exceeded.");
	 return nil;
  }
  cons stk = {.car = value, .cdr = lisp_stack};
  lisp_stack = cons_lisp_value(&stk);
  lisp_stack_size += 1;
  
  var r = lisp_eval_inner(scope, value);
  lisp_stack = stk.cdr;
  lisp_stack_size -= 1;
  return r;
}

static inline lisp_value lookup_local_index(lisp_scope * scope, lisp_local_index index){
  while(index.scope_level > 0){
    scope = scope->super;
    ASSERT(scope != NULL);
    index.scope_level -= 1;
  }
  if(index.scope_type == 0){
    cons v = scope->lookup[index.scope_index];
    return v.cdr;
  }
  return scope->values[index.scope_index];  
}

lisp_value lisp_eval_inner(lisp_scope * scope, lisp_value value){
  
  switch(lisp_value_type(value)){
  case LISP_CONS:
	{
      var first = car_nocheck(value);
	  var first_type = lisp_value_type(first);
      lisp_scope * s1;
      int i1, i2;
      switch(first_type){
      case LISP_SYMBOL:
        {
          if(lisp_scope_try_get_value2(scope, first.integer, &s1, &i1, &i2))
            {
              if(i2 == -1){
                first = s1->values[i1];
              }else{
                first = s1->lookup[i2].cdr;
              }
              if(s1 == current_context->globals){
                
                lisp_value new = global_index_lisp_value(i1);
                set_car(value, new);
                
                var first_value2 = lisp_eval(scope, new);
                ASSERT(lisp_value_eq(first, first_value2));
                first = first_value2;
              }
              // this is a performance optimization for the normal case - calling a function by name
            }
		  first_type = lisp_value_type(first);
		  
        }
        break;
      case LISP_FUNCTION:
        break;
      case LISP_FUNCTION_NATIVE:
        break;
      case LISP_GLOBAL_INDEX:
        first = current_context->globals->values[first.integer];
		first_type = lisp_value_type(first);
      
        break;
      case LISP_LOCAL_INDEX:
        first = lookup_local_index(scope, first.local_index);
        first_type = lisp_value_type(first);
      break;
      default:
        first = lisp_eval(scope, first);
        first_type = lisp_value_type(first);
      break;
      }
      
	  if(first_type == LISP_MACRO_BUILTIN){
		switch(first.builtin){
		case LISP_IF:
		  return lisp_eval_if(scope, cadr(value), caddr(value), cadddr(value));
		case LISP_QUOTE:
		  if(first.builtin == LISP_QUOTE)
			return cadr(value);
		  goto quasiquote;
		case LISP_QUASIQUOTE:
		quasiquote:
		  return lisp_eval_quasiquoted(scope, cadr(value));
		case LISP_UNQUOTE_SPLICE:
		case LISP_UNQUOTE:
		  raise_string("Unexpected unquote!\n");
		  break;
		case LISP_LET:
		  return lisp_eval_let(scope, cadr(value), cddr(value));
		case LISP_PROGN:
          return lisp_eval_progn(scope, cdr(value));
		case LISP_LOOP:
          return lisp_eval_loop(scope, cadr(value), cddr(value));
		case LISP_LAMBDA:
          return lisp_eval_lambda(scope, cadr(value), cddr(value));
		case LISP_MACRO:
          return lisp_eval_macro(scope, cadr(value), cddr(value));
        case LISP_SET:
          return lisp_eval_set(scope, cdr(value));
		case LISP_DEFINE:
          return lisp_eval_define(scope, cadr(value), caddr(value));
        case LISP_SYMBOL_VALUE:
          return lisp_eval_symbol_value(scope, cadr(value), caddr(value));
	
        case LISP_BOUND:
          return lisp_eval_boundp(scope, cadr(value), caddr(value));          
        case LISP_WITH_EXCEPTION_HANDLER:
          return lisp_eval_with_exception_handler(scope, cadr(value), caddr(value));
        case LISP_CASE:
          return lisp_eval_case(scope, cadr(value), cddr(value));
        case LISP_COND:
          return lisp_eval_cond(scope, cdr(value));
        case LISP_AND:
          return lisp_eval_and(scope, cdr(value));
        case LISP_OR:
          return lisp_eval_or(scope, cdr(value));
        case LISP_GET_SCOPE:
          return lisp_eval_get_scope(scope);
        case LISP_REF:
          return lisp_eval_ref(scope, cadr(value));
            
        case LISP_GET_SCOPE_UNSAFE:
          return lisp_eval_get_scope_unsafe(scope);
        case LISP_CAR:
          return car(lisp_eval(scope, cadr(value)));
        case LISP_CDR:
          return cdr(lisp_eval(scope, cadr(value)));
        case LISP_CDDR:
          return cddr(lisp_eval(scope, cadr(value)));
        case LISP_CADR:
          return cadr(lisp_eval(scope, cadr(value)));
        case LISP_EQN:
          return lisp_eval_eqn(scope, cdr(value));
        case LISP_IS_SYMBOL:
          return lisp_is_symbol(lisp_eval(scope, cadr(value)));
        case LISP_IS_LIST:
          return lisp_is_list(lisp_eval(scope, cadr(value)));
        case LISP_PLOOKUP:
          return lisp_eval_plookup(scope, cadr(value), caddr(value));
        case LISP_NEW_CONS:
          return new_cons(lisp_eval(scope, cadr(value)), lisp_eval(scope, caddr(value)));
        case LISP_ADD:
          return lisp_eval_add(scope, cdr(value));
        case LISP_SUB:
          return lisp_eval_sub(scope, cdr(value));
        case LISP_MUL:
          return lisp_eval_mul(scope, cdr(value));
        case LISP_VECTOR_LENGTH:
          return lisp_eval_vector_length(scope, cadr(value));
        case LISP_VECTOR_REF:
          return lisp_eval_vector_ref(scope, cadr(value), caddr(value));
        case LISP_VECTOR_SET:
          return lisp_eval_vector_set(scope, cadr(value), caddr(value), cadddr(value));
        case LISP_CONV_FLOAT32:
          return lisp_float32(lisp_eval(scope, cadr(value)));
        }
	  }
	  size_t argcnt = lisp_optimize_statement(scope, value) - 1;
      
	  if(lisp_is_in_error())
		return nil;
		
	  switch(first_type){
	  case LISP_FUNCTION:
		return lisp_eval_function(scope, lisp_value_function(first), argcnt, cdr(value));
	  case LISP_FUNCTION_NATIVE:
		return lisp_eval_native_functions(scope, lisp_value_native_function(first), argcnt, cdr(value));
	  default:
		lisp_error(new_cons(value, string_lisp_value("is not a function")));
		return nil;
	  }     
	}
	break;
  case LISP_SYMBOL:
    return lisp_eval_symbol(scope, value);
  case LISP_GLOBAL_INDEX:
    return current_context->globals->values[value.integer];
  case LISP_LOCAL_INDEX:
    return lookup_local_index(scope, value.local_index);
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
	bool p = gc_unsafe_stack;
	gc_unsafe_stack = true;
    var next_code = lisp_macro_expand(scopeptr, code);
	gc_unsafe_stack = p;
    if(lisp_value_eq(next_code, code))
      break;
    code = next_code;
  }
  
  return lisp_eval(scopeptr, code);
}

void on_read_cons(io_reader * rd, lisp_value c){
  if(!is_nil(read_cons_offset)){
    int offset = rd->offset;
    lisp_hashtable_set(read_cons_offset, c, integer_lisp_value(offset));
	
  }
  if(!is_nil(read_current_file) && !is_nil(read_cons_file))
    lisp_hashtable_set(read_cons_file, c, read_current_file);
}

lisp_value lisp_code_location(lisp_value cons){
  if(is_nil(read_cons_offset)) return nil;
  var offset = lisp_hashtable_get(read_cons_offset, cons);
  var file = lisp_hashtable_get(read_cons_file, cons);

  if(lisp_value_type(offset) != LISP_INTEGER)
	return nil;
  
  if(lisp_value_type(file) != LISP_STRING)
    return nil;
  var filepath = lisp_value_string(file); 
  var code = read_file_to_string(filepath);
  if(code == NULL) return nil;
  var ptr = code;
  int lines = 0;
  int col = 0;
  int offs = lisp_value_integer(offset);
  while(*code && (offs-- > 0)){
    if(*code == '\n'){
      lines++;
      col = 0;
    }else{
      col++;
    }
    code++;
  }
  if(*code){
    // found line
    dealloc(ptr);
    return new_cons(file, new_cons(integer_lisp_value(lines + 1), new_cons(integer_lisp_value(col), nil)));
  }

  dealloc(ptr);
  return nil;
}

lisp_value lisp_print_code_location(lisp_value cons){
  
  if(is_nil(read_cons_offset))
    return nil;
  var offset = lisp_hashtable_get(read_cons_offset, cons);
  var file = lisp_hashtable_get(read_cons_file, cons);
  if(lisp_value_type(offset) != LISP_INTEGER)
    return nil;
  if(lisp_value_type(file) != LISP_STRING)
    return nil;
  var filepath = lisp_value_string(file); 
  var code = read_file_to_string(filepath);
  if(code == NULL) return nil;
  var ptr = code;
  int lines = 0;
  int col = 0;
  int offs = lisp_value_integer(offset);
  while(*code && (offs-- > 0)){
    if(*code == '\n'){
      lines++;
      col = 0;
    }else{
      col++;
    }
    code++;
  }
  if(*code){
    // found line
    dealloc(ptr);
    printf("%s:%i:%i", filepath, lines + 1, col);
    return t;
  }

  dealloc(ptr);
  return nil;
}

lisp_value current_toplevel = {0};

lisp_value lisp_eval_stream(io_reader * rd){
  lisp_value result = nil;
  lisp_value next_toplevel = {0};
  cons toplevel = {.car = current_toplevel,
				   .cdr = lisp_pointer_to_lisp_value(&next_toplevel)};
  current_toplevel = cons_lisp_value(&toplevel);
  
  while(true){
	current_toplevel = nil;
	gc_collect_garbage(current_context);
	
	var off = rd->offset;

	var p = gc_unsafe_stack;
	gc_unsafe_stack = true;
	  
	var code = lisp_read_stream(rd);
	gc_unsafe_stack = p;	
	
	if(off == rd->offset || is_nil(code)) break;
    while(true){
      if(lisp_is_in_error()){
		current_toplevel = toplevel.car;
        return nil;
	  }
      var p = gc_unsafe_stack;
	  gc_unsafe_stack = true;
	  next_toplevel = code;
	  var next_code = lisp_macro_expand(current_context->globals, code);
	  gc_unsafe_stack = p;	
	  
	  if(lisp_value_eq(next_code, code))
        break;
      code = next_code;

	}

	next_toplevel = code;
    result = lisp_eval(current_context->globals, code);
	current_toplevel = toplevel.car;
    	
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


lisp_value read_current_files = {0};

lisp_value lisp_eval_file(const char * filepath){
  char * buffer = read_file_to_string(filepath);
  println(read_current_file);
  read_current_file = lisp_string(filepath);
  read_current_files = new_cons(read_current_file, read_current_files);

  if(buffer == NULL) {
    printf("FILE: %s\n", filepath);
    raise_string("Unable to read file");
    return nil;
  }
  lisp_value r = lisp_eval_string(buffer);
  free(buffer);
  read_current_files = cdr(read_current_files);
  read_current_file = car(read_current_files);
  
  return r;
}
static int lisp_print_depth = 10;
bool lisp_string_double_quotes = true;
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
    return snprintf(buffer, LEN1, "%lli", (long long)v.integer);
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
      return snprintf(buffer, LEN1, "%p", v.native_pointer);
  case LISP_FLOAT32:
  case LISP_RATIONAL:
    return snprintf(buffer, LEN1, "%g", v.rational);
  case LISP_STRING:
	 if(lisp_string_double_quotes)
		return snprintf(buffer, LEN1, "\"%s\"", v.string);
	 else
		return snprintf(buffer, LEN1, "%s", v.string);
  case LISP_SYMBOL:
    return snprintf(buffer, LEN1, "%s", symbol_name(v.integer));
  case LISP_FUNCTION:
    return snprintf(buffer, LEN1, "FUNCTION %p", v.native_pointer);
  case LISP_FUNCTION_MACRO:
    return snprintf(buffer, LEN1, "FUNCTION_MACRO");
  case LISP_FUNCTION_NATIVE:
    return snprintf(buffer, LEN1, "Native function");
  case LISP_MACRO_BUILTIN:
    return snprintf(buffer, LEN1, "MacroBuiltin");
  case LISP_HASHTABLE:
    return snprintf(buffer, LEN1, "HashTable(%i)", (int)lisp_value_hashtable(v)->count);
  case LISP_SCOPE:
    return snprintf(buffer, LEN1, "Scope (%i) %p", (int)lisp_value_scope(v)->argcnt, lisp_value_scope(v));
  case LISP_GLOBAL_INDEX:
    {
      var sym = current_context->globals->value_symbol[v.integer];
      return snprintf(buffer, LEN1, "%s", symbol_name(sym));
    }
  case LISP_LOCAL_INDEX:
    return snprintf(buffer, LEN1, "[Local Index (%i,%i)]",v.local_index.scope_level, v.local_index.scope_index);
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
        lisp_print_depth--;
        if(lisp_print_depth > 0)
          l += print2(OBUF, LEN1, v.cons->car);
        else
          l += snprintf(OBUF, LEN1, ".");
        lisp_print_depth++;
        v = v.cons->cdr;
      }
      if(!is_nil(v)){
        l += snprintf(OBUF, LEN1, " . ");
        l += print2(OBUF, LEN1, v);
      }
      l += snprintf(OBUF, LEN1, ")");
      return l;
    }
  case LISP_ALLOCATED_POINTER:
    l += snprintf(OBUF, LEN1, "[GC'd pointer %p]", lisp_value_pointer(v));
    return l;
  case LISP_NATIVE_POINTER_TO_VALUE:
    {
      l += snprintf(OBUF, LEN1, "*[");
      lisp_value * val = lisp_value_pointer(v);
      if(val != NULL)
        l += print2(OBUF, LEN1, *val);
      else
        l += snprintf(OBUF, LEN1, "NULL");
      l += snprintf(OBUF, LEN1, "]");
      
      return l;
    }
  case LISP_GLOBAL_CONS_ARRAYS:
	snprintf(OBUF, LEN1, "[global cons array %i]", (int)  ((t_cons_arrays *)lisp_value_pointer(v))->count);
	return l;
  case LISP_VALUE_SET:
	snprintf(OBUF, LEN1, "[lisp_value_set ?]");
	return l;
  case LISP_ARRAY:
	snprintf(OBUF, LEN1, "[lisp_array %i]", (int)((lisp_array *) lisp_value_pointer)->count);
	return l;
  }
  return 0;
}


lisp_value println_shallow(lisp_value v){
  int p = lisp_print_depth;
  lisp_print_depth = 2;
  println(v);
  lisp_print_depth = p;
  return nil;
}

lisp_value print(lisp_value v){
  char buffer[100] = {0};
  int l = print2(NULL,0,  v);
  char * str = l >= 95 ? malloc(l + 1) : buffer;
  if(str == NULL){
	printf("Unable to allocate %i\n", l + 1);
	return integer(0);
  }
  print2(str, l+ 1, v);
  printf("%s", str);
  if(str != buffer)
    free(str);
  return integer(l);
}

lisp_value value_to_string(lisp_value v){
  int l = print2(NULL,0,  v);
  l += 100;
  char * str = lisp_malloc(l+ 1);
  lisp_string_double_quotes = false;
  print2(str, l + 1, v);
  var r = string_lisp_value(str);
  lisp_string_double_quotes = true;
  return r;
}

void lisp_register_value(const char * name, lisp_value value){
  lisp_scope_create_value(current_context->globals, get_symbol(name), value);
}
void lisp_register_native(const char * name, int nargs, void * fptr){
  native_function *nf = lisp_malloc(sizeof(*nf));
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
        return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum); \
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
      return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum); \
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
      return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);;
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
    return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);
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

bool equals(lisp_value v1, lisp_value v2){
  var t1 = lisp_value_type(v1);
  var t2 = lisp_value_type(v2);
  if(t1 != t2) return false;
  if(memcmp(&v1, &v2, sizeof(lisp_value)) == 0) return true;
  switch(t1)
    {
    case LISP_CONS:
      {
        var c1 = lisp_value_cons(v1);
        var c2 = lisp_value_cons(v2);
        return equals(c1->car, c2->car) && equals(c1->cdr, c2->cdr);
      }
    case LISP_STRING:
      return strcmp(lisp_value_string(v1), lisp_value_string(v2));
    default:
      return v1.integer == v2.integer;
    }
}

lisp_value lisp_equals(lisp_value * values, int n){
  if(n == 0) return nil;
  for(int i = 1; i < n; i++){
    if(!equals(values[i - 1], values[i]))
      return nil;
  }
  return t;
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
lisp_value lisp_println(lisp_value * values, int count){
  for(int i = 0; i < count; i++)
	 print(values[i]);
  printf("\n");
  return count == 0 ? nil : values[0];
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

lisp_value lisp_cos(lisp_value v){
  return rational_lisp_value(cos(lisp_value_as_rational(v)));
}
lisp_value lisp_abs(lisp_value v){
  return rational_lisp_value(fabs(lisp_value_as_rational(v)));
}

lisp_value lisp_mod(lisp_value a, lisp_value b){
  if(is_float(a) || is_float(b))
    return rational_lisp_value(fmod(lisp_value_as_rational(a), lisp_value_as_rational(b)));
  return integer_lisp_value(lisp_value_as_integer(a) % lisp_value_as_integer(b));
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
  case LISP_ALLOCATED_POINTER: return "ALLOCATED_POINTER";
  case LISP_NATIVE_POINTER_TO_VALUE: return "NATIVE_POINTER_TO_VALUE";
  case LISP_GLOBAL_CONS_ARRAYS: return "GLOBAL_CONS_ARRAY";
  case LISP_VALUE_SET: return "LISP_VALUE_SET";
  case LISP_ARRAY: return "LISP_ARRAY";
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
    vector_set(v, integer_lisp_value(i), _default);
    
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
    v.rational = (double)((float *) vector->data)[i];
    return v;
  }else
    dst = &v.integer;
  memcpy(dst, src, vector->elem_size);
  
  return v;
}

lisp_value vector_set(lisp_value _vector, lisp_value k, lisp_value value){
  TYPE_ASSERT(_vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var i = (size_t)lisp_value_integer(k);
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
  var vec0 = lisp_value_vector(vector);
  lisp_vector * vec = lisp_malloc(sizeof(*vec));
  *vec = *vec0;
  size_t elem_size = lisp_type_size(lisp_value_type(vec->default_value));
  
  void * new_data = lisp_malloc(l * elem_size);
  size_t prevCount = MIN(l, vec->count);   
  memcpy(new_data, vec->data, prevCount * elem_size);
  vec->data = new_data;
  vec->count = l;
  //for(size_t i = prevCount; i < l; i++){
  //  vector_set(vector, integer_lisp_value(i), vec->default_value);
  //}

  return vector_lisp_value(vec);
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
  int cnt = snprintf(NULL, 0, "%x", (u32)v);
  if(is_nil(dec))
    l = cnt;
  char * buf = lisp_malloc(l + 1);
  snprintf(buf + MAX(0, l - cnt), MIN(cnt, l) + 1, "%x", (u32)v);
  buf[l] = 0;
  for(int i = 0; i < MAX(0, l - cnt); i++)
    buf[i] = '0';
  return string_lisp_value(buf);
}

lisp_value lisp_string(const char *str ){
  return string_lisp_value(gc_clone(str, strlen(str) + 1));
}

lisp_value lisp_signature(lisp_value func){
  if(is_function(func))
    return new_cons(get_symbol("func"), func.function->args);
  return nil;
}
int lisp_value_hash(lisp_value v){
  switch(lisp_value_type(v)){
  case LISP_CONS:
    {
      cons * c = lisp_value_cons(v);
      int hash = lisp_value_hash(c->car);
      hash = hash ^ lisp_value_hash(c->cdr);
      hash *= 16777619;
      return hash;
    }
  default:;
    
  }
  int hash = (i32)2166136261;
  hash = hash ^ v.type;
  hash *= 16777619;
  hash = hash ^ v.integer;
  hash *= 16777619;
  return hash;
}
static int lisp_value_full_hash(const void * _key_data, void * userdata){
  UNUSED(userdata);
  const lisp_value * key_data = _key_data;
  int h = lisp_value_hash(*key_data);
  return h;
}

static bool lisp_value_full_compare(const void * _k1, const void * _k2, void * userdata){
  UNUSED(userdata);
  const lisp_value * k1 = _k1, * k2 = _k2 ;
  return equals(*k1, *k2);
}
void ht_init(hash_table * ht);


// TODO: add support for GCing >1 columns of the hashtable key.
lisp_value lisp_make_hashtable(lisp_value * args, int n){
  static lisp_value full_keyword;
  static lisp_value weak_keyword;
  static lisp_value keys_keyword;
  bool full = false;
  bool weak = false;
  int keys = 1;
  for(int i = 0; i < n; i++){
    lisp_value arg = args[i];
    if(eq(arg, get_symbol_cached(&full_keyword, ":deep-equality")))
      full = true;
    
    if(eq(arg, get_symbol_cached(&weak_keyword, ":weak")))
      weak = true;
	if(eq(arg, get_symbol_cached(&keys_keyword, ":keys"))){
	  if(i + 1 == n) {
		raise_string("invalid use of :keys");
		return nil;
	  }
	  keys = args[i + 1].integer;
	  i += 1;
	}
  }

  hash_table * ht = lisp_malloc(sizeof(*ht));
  ht_create3(ht, 4, sizeof(lisp_value) * keys, sizeof(lisp_value));
  
  ht_set_alloc(ht, lisp_malloc, lisp_free);
  if(full){
    ht->hash = lisp_value_full_hash;
    ht->compare = lisp_value_full_compare;
  }
  if(weak){
    ((void **) &ht->userdata)[0] = (void *) (size_t)LISP_HASHTABLE_WEAK_KEYS;  
  }
  ht_init(ht);
  return hashtable_lisp_value(ht);
}

lisp_value lisp_make_hashtable0(){
  return lisp_make_hashtable(NULL, 0);
}

lisp_value lisp_make_hashtable_weak_keys(){
  hash_table * ht = lisp_malloc(sizeof(*ht));
  ht_create3(ht, 8, sizeof(lisp_value), sizeof(lisp_value));
  ht_set_alloc(ht, lisp_malloc, lisp_free);
  ((void **) &ht->userdata)[0] = (void *) (size_t)LISP_HASHTABLE_WEAK_KEYS;  
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

lisp_value lisp_hashtable_setn(lisp_value * values, size_t count){
  if(count <= 2){
	raise_string("not enough values for set");
	return nil;
  }
  
  var _ht = values[0];
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  ht_set(ht, values + 1, values + count - 1);
  return nil;
}


lisp_value lisp_hashtable_getn(lisp_value * values, size_t count){
  if(count <= 1){
	raise_string("not enough values for get");
	return nil;
  }

  lisp_value ht = values[0];
  lisp_value value = {0};
 				 
  if(ht_get(lisp_value_hashtable(ht), &values[1], &value))
    return value;
  return nil;
}
lisp_value lisp_hashtable_count(lisp_value ht){
  TYPE_ASSERT(ht, LISP_HASHTABLE);
  var ht2 = lisp_value_hashtable(ht);
  return integer_lisp_value(ht2->count);
}

void it_symbols(void * k1, void * k2, void * target){
  UNUSED(k1);
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

static void iter_add_key(void * key, void * elem, void * user_data){
  UNUSED(elem);
  lisp_value * ud = user_data;
  *ud = new_cons(((lisp_value *) key)[0], *ud);
}

lisp_value lisp_hashtable_keys(lisp_value _ht){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  lisp_value keys = nil;
  ht_iterate(ht, iter_add_key, &keys);
  return keys;
}

static void iter_add_value(void * key, void * elem, void * user_data){
  UNUSED(key);
  lisp_value * ud = user_data;
  printf("???\n");
  *ud = new_cons(((lisp_value *) elem)[0], *ud);
}

lisp_value lisp_hashtable_values(lisp_value _ht){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  lisp_value keys = nil;
  ht_iterate(ht, iter_add_value, &keys);
  return keys;
}

lisp_value lisp_hashtable_clear(lisp_value _ht){
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table * ht = lisp_value_hashtable(_ht);
  ht_clear(ht);
  return nil;
}


lisp_value lisp_is_symbol(lisp_value v){
  if(is_symbol(v))
    return t;
  return nil;
}

lisp_value lisp_is_integer(lisp_value v){
  if(is_integer(v))
    return t;
  return nil;
}

lisp_value lisp_is_rational(lisp_value v){
  if(is_float(v))
    return t;
  return nil;
}

lisp_value lisp_is_number(lisp_value v){
  if(is_float(v) || is_integer(v))
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
void gc_register();
void load_modules(){
  gc_register();
  foxgl_register();
  lisp_process_module_init();
}

void web_update(){
  var sym = get_symbol("lisp:*web-update*");
  lisp_eval(current_context->globals, new_cons(sym, nil));
}

void sig_handler(int signum){
  printf("Caught signal! %i\n", signum);
  current_error = fpe_sym;
  siglongjmp(error_resume_jmp, 1);
}


void setup_fpe_handler(){
  struct sigaction new_action;
  
  struct sigaction old_action;
  new_action.sa_handler = sig_handler;
  //new_action.sa_sigaction = sig_handler2;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags =SA_NODEFER | SA_NOMASK;
  
  sigaction (SIGFPE, &new_action, &old_action);
}

void test_gc();
void test_sdf();
int main(int argc, char ** argv){
  for(int i = 1; i < argc; i++){
    if(strcmp(argv[i], "--test-gc") == 0){
      test_gc();
      return 0;
    }
  }
  
  current_context = lisp_context_new();

  for(int i = 1; i < argc; i++){
  if(strcmp(argv[i], "--test-sdf") == 0){
      test_sdf();
      return 0;
    }
  }
  
  
  setup_fpe_handler();
  
  lisp_register_value("lisp:*test-enabled*", nil);
  
#ifndef WASM
  for(int i = 1; i < argc; i++){
    if(strcmp(argv[i], "--test") == 0){
      lisp_register_value("lisp:*test-enabled*", t);
    }else
      lisp_eval_file(argv[i]);
  }
#endif

#ifdef WASM
  lisp_eval_file("foxday2.lisp");

  emscripten_set_main_loop(web_update, 0, 1);
  
#endif
  if(lisp_is_in_error()){
    printf("Exiting due to unhandled exception\n");
    var stk = current_error_stack;
	
    while(!is_nil(stk)){
      println(car(stk));
      stk = cdr(stk);
    }
    println(current_error);
    
  }
  return 0;
}

#ifdef WASM
EMSCRIPTEN_KEEPALIVE
void * lisp_read_file(const char * filename){
  var s = read_file_to_string(filename);
  return s;
}
EMSCRIPTEN_KEEPALIVE
void lisp_invoke_string(const char * code){
  lisp_eval_string(code);
}

#endif

lisp_value lisp_debug(lisp_value v){
  debug_enabled = !is_nil(v);
  return nil;
}

extern void * (* ht_mem_malloc)(size_t s);
extern void (* ht_mem_free)(void *);

lisp_context * lisp_context_new_bare(){
  lisp_context * ctx = alloc0(sizeof(ctx[0]));
  ctx->gc = gc_context_new();
 
  ctx->next_symbol = 1;
  ctx->symbols = ht_create_strkey(sizeof(u64));
  ctx->symbols_reverse = ht_create(sizeof(u64), sizeof(char *));
  ctx->symbols_reverse->hash = symbol_nohash;
  var prevctx = current_context;
  current_context = ctx;
  ctx->globals = lisp_scope_new(NULL);
  current_context = prevctx;
  return ctx;
}

lisp_context * lisp_context_new(){
  
  lisp_context * ctx = lisp_context_new_bare();
  current_context = ctx;

  //setup const symbols
  rest_sym = get_symbol("&rest");
  if_sym = get_symbol("if");
  quote_sym = get_symbol("quote");
  quasiquote_sym = get_symbol("quasiquote");
  unquote_sym = get_symbol("unquote");
  else_sym = get_symbol("else");
  otherwise_sym = get_symbol("otherwise");
  unquote_splice_sym = get_symbol("unquote-splicing");
  fpe_sym = get_symbol("floating-point-error");
  current_error_sym = get_symbol("lisp:+current-error+");


  lisp_scope_create_value(current_context->globals, get_symbol("nil"), nil);
  lisp_scope_create_value(current_context->globals, get_symbol("t"), t);

  load_lisp_base();
  lisp_register_native("lisp:count-allocated", 0, lisp_count_allocated);
  lisp_register_native("lisp:exit", 0, lisp_exit);
  lisp_register_native("lisp:trace", 1, lisp_trace);
  lisp_register_native("integer?", 1, lisp_is_integer);
  lisp_register_native("number?", 1, lisp_is_number);
  lisp_register_native("rational?", 1, lisp_is_rational);
  //lisp_register_native("+", -1, lisp_addn);
  //lisp_register_native("-", -1, lisp_subn);
  //lisp_register_native("*", -1, lisp_muln);
  lisp_register_native("/",-1, lisp_divn);
  lisp_register_native("<", -1, lisp_lessn);
  lisp_register_native(">", -1, lisp_greatern);
  lisp_register_native("<=", -1, lisp_lesseqn);
  lisp_register_native(">=", -1, lisp_greatereqn);
  lisp_register_native("min", -1, lisp_minimumn);
  lisp_register_native("max", -1, lisp_maximumn);

  lisp_register_native("print", 1, print);
  lisp_register_native("println", -1, lisp_println);
  lisp_register_native("value->string", 1, value_to_string);
  //lisp_register_native("car", 1, car);
  //lisp_register_native("cdr", 1, cdr);
  //lisp_register_native("cddr", 1, cddr); // macro
  lisp_register_native("cadr", 1, cadr);
  lisp_register_native("set-car!", 2, set_car);
  lisp_register_native("set-cdr!", 2, set_cdr);
  //lisp_register_native("cons", 2, new_cons);
  lisp_register_native("length", 1, lisp_length);
  lisp_register_native("lisp:collect-garbage", 0, lisp_collect_garbage);
  lisp_register_native("lisp:get-allocated", 0, lisp_get_allocated);
  lisp_register_native("lisp:trace-allocations", 1, lisp_trace_allocations);
  lisp_register_native("lisp:debug", 1, lisp_debug);
  lisp_register_native("copy-list", 1, copy_cons);
  lisp_register_native("copy-list-deep", 1, copy_cons_deep);
  //lisp_register_native("plookup", 2, lisp_plookup);

  lisp_register_native("equal?", -1, lisp_equals);
  //lisp_register_native("=", -1, lisp_value_eqn);
  lisp_register_native("/=", -1, lisp_neqn);
  lisp_register_native("panic", 1, lisp_error);
  lisp_register_native("integer", 1, lisp_integer);
  lisp_register_native("rational", 1, lisp_rational);
  //lisp_register_native("float32", 1, lisp_float32);
  lisp_register_native("byte", 1, lisp_byte);
  lisp_register_native("sin", 1, lisp_sin);
  lisp_register_native("cos", 1, lisp_cos);
  lisp_register_native("abs", 1, lisp_abs);
  lisp_register_native("mod", 2, lisp_mod);
  lisp_register_native("type-of", 1, lisp_type_of);
  
  lisp_register_native("read-string", 1, lisp_read);
  lisp_register_native("load", 1, lisp_load);
  
  lisp_register_native("make-vector", 2, make_vector);
  //lisp_register_native("vector-ref", 2, vector_ref);
  //lisp_register_native("vector-set!", 3, vector_set);
  lisp_register_native("vector-element-type", 1, vector_elem_type);
  //lisp_register_native("vector-length", 1, vector_length);
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

  lisp_register_native("make-hashtable", -1, lisp_make_hashtable);
  lisp_register_native("hashtable-ref", 2, lisp_hashtable_get);
  lisp_register_native("hashtable-set", 3, lisp_hashtable_set);
  lisp_register_native("hashtable-refn", -1, lisp_hashtable_getn);
  lisp_register_native("hashtable-setn!", -1, lisp_hashtable_setn);
  lisp_register_native("hashtable-remove", 2, lisp_hashtable_remove);
  lisp_register_native("hashtable-ref2", 2, lisp_hashtable_get2);
  lisp_register_native("hashtable-keys", 1, lisp_hashtable_keys);
  lisp_register_native("hashtable-values", 1, lisp_hashtable_values);
  lisp_register_native("hashtable-clear", 1, lisp_hashtable_clear);
  lisp_register_native("hashtable-count", 1, lisp_hashtable_count);
  lisp_register_native("function-signature", 1, lisp_signature);

  lisp_register_native("optimize-statement", 2, _lisp_optimize_statement);
  lisp_register_native("function->code", 1, lisp_function_code);
  lisp_register_native("lisp:sub-scope", 3, lisp_sub_scope);
  lisp_register_native("lisp:with-sub-scope", 4, lisp_with_sub_scope);
  lisp_register_native("lisp:with-scope-binding", 5, lisp_with_scope_binding);
  lisp_register_native("lisp:with-scope-variable", 5, lisp_with_scope_vars);
  lisp_register_native("lisp:scope-vars", 1, lisp_scope_vars);
  lisp_register_native("lisp:scope-super", 1, lisp_scope_super);
  lisp_register_native("lisp:scope-set!", 3, lisp_scope_set);
  lisp_register_native("lisp:code-location", 1, lisp_code_location);
  lisp_register_native("deref-pointer", 1, lisp_value_deref);
  lisp_register_native("eval", 2, lisp_eval_value);
  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  lisp_register_macro("+let-impl+", LISP_LET);
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
  lisp_register_macro("lisp:get-current-scope!!", LISP_GET_SCOPE_UNSAFE);
  
  lisp_register_macro("ref!!", LISP_REF);

  lisp_register_macro("car", LISP_CAR);
  lisp_register_macro("cdr", LISP_CDR);
  lisp_register_macro("cddr", LISP_CDDR);
  lisp_register_macro("cadr", LISP_CADR);
  lisp_register_macro("=", LISP_EQN);
  lisp_register_macro("symbol?", LISP_IS_SYMBOL);
  lisp_register_macro("list?", LISP_IS_LIST);
  lisp_register_macro("plookup", LISP_PLOOKUP);
  lisp_register_macro("cons", LISP_NEW_CONS);
  lisp_register_macro("+", LISP_ADD);
  lisp_register_macro("-", LISP_SUB);
  lisp_register_macro("*", LISP_MUL);
  lisp_register_macro("vector-length", LISP_VECTOR_LENGTH);
  lisp_register_macro("vector-ref", LISP_VECTOR_REF);
  lisp_register_macro("vector-set!", LISP_VECTOR_SET);
  lisp_register_macro("float32", LISP_CONV_FLOAT32);
  
  
  lisp_register_value("native-null-pointer", (lisp_value){.type = LISP_NATIVE_POINTER, .integer = 0});
#ifndef WASM
  lisp_register_value("lisp:*web-environment*", nil);
#else
  lisp_register_value("lisp:*web-environment*", t);
  lisp_register_value("lisp:*web-update*", nil);
#endif

  // keeping track of debug information
  // to optimize: insert nil here instead.
  read_cons_offset = nil;//lisp_make_hashtable_weak_keys();
  read_cons_file = nil;//lisp_make_hashtable_weak_keys();
  
  lisp_register_value("lisp:++cons-file-offset++", read_cons_offset);
  lisp_register_value("lisp:++cons-file-offset2++", lisp_pointer_to_lisp_value(&read_cons_offset));
  lisp_register_value("lisp:++cons-file++", read_cons_file);
  lisp_register_value("lisp:++cons-file2++", lisp_pointer_to_lisp_value(&read_cons_file));

  lisp_register_value("lisp:++stack++", lisp_pointer_to_lisp_value(&lisp_stack));
  lisp_register_value("lisp:++current-error++", lisp_pointer_to_lisp_value(&current_error));
  lisp_register_value("lisp:++current-error-stack++", lisp_pointer_to_lisp_value(&current_error_stack));
  lisp_register_value("lisp:++current-file-ptr++", lisp_pointer_to_lisp_value(&read_current_file));
  lisp_register_value("lisp:++current-files-ptr++", lisp_pointer_to_lisp_value(&read_current_files));

  lisp_register_value("lisp:++current-toplevel-ptr++", lisp_pointer_to_lisp_value(&current_toplevel));
  
  

  lisp_value cons_arrays_val = native_pointer_lisp_value(&cons_arrays);
  cons_arrays_val.type = LISP_GLOBAL_CONS_ARRAYS;
  lisp_register_value("lisp:++pinned_cons_arrays++", cons_arrays_val );
  
  load_modules();

  return ctx;
}
