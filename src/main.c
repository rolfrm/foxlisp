#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>
#define GC_REDIRECT_TO_LOCAL
#define GC_THREADS
#include <gc.h>

#ifdef EMSCRIPTEN
#include "emscripten.h"
#endif

#include "foxlisp.h"

extern bool debug_set;
bool is_float(lisp_value a){
  return a.type == LISP_RATIONAL || a.type == LISP_FLOAT32;
}
lisp_value rest_sym = {.type = LISP_SYMBOL};
lisp_value if_sym = {.type = LISP_SYMBOL};
lisp_value quote_sym = {.type = LISP_SYMBOL};
lisp_value quasiquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_splice_sym = {.type = LISP_SYMBOL};
lisp_value nil = {0};
lisp_value t = {.type = LISP_T};

#undef ASSERT
void ht_free(hash_table *ht);
extern void * (* ht_mem_malloc)(size_t s);
extern void (* ht_mem_free)(void *);
void ht_set_mem_values(hash_table * ht, void * (* alloc)(size_t s), void (* free)(void * ptr));
void ht_set_mem_keys(hash_table * ht, void * (* alloc)(size_t s), void (* free)(void * ptr));
void ht_empty(hash_table *ht);
void print_call_stack();
static __thread lisp_value current_error;
bool lisp_is_in_error(){
  return !is_nil(current_error);
}

void raise_string(const char * str){
  current_error = (lisp_value){.string = str, .type = LISP_STRING};
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
  if(val.type != type){
    char buffer[1000];
	 sprintf(buffer, "Invalid type, expected %s, but got %s\n",
			  lisp_type_to_string(type),
			  lisp_type_to_string(val.type));
	 raise_string(gc_clone(buffer, strlen(buffer)));
    return false;
  }
  return true;
}

bool elem_type_assert(lisp_value vector, lisp_type type){
  return type_assert(vector.vector->default_value, type);
}


#define ASSERT(x) if(!x){printf("!!! %s\n",  #x); error(); }

bool is_nil(lisp_value v){
  return v.type == LISP_NIL;
}

bool is_integer(lisp_value v){
  return v.type == LISP_INTEGER;
}

bool  _lisp_eq(lisp_value a, lisp_value b){
  if(a.type != b.type) return false;
  if(is_float(a)) return a.rational == b.rational;
  return a.integer == b.integer;
}

bool  string_eq(lisp_value a, lisp_value b){
  if(a.type != b.type) return false;
  if(a.type != LISP_STRING) return false;
  return strcmp(a.string, b.string) == 0;
}

void * gc_clone(const void * mem, size_t s){
  void * d = lisp_malloc(s);
  memcpy(d, mem, s);
  return d;
}

lisp_scope * lisp_scope_new(lisp_scope * super){
  lisp_scope * s = lisp_malloc(sizeof(*super));
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
lisp_scope * lisp_scope_unstack(lisp_scope * scope){

  if(scope->super == NULL)
    return scope;
  if(scope->stack_scope == false)
    return scope;
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
  if(scope == NULL) return nil;
  lisp_value r;
  if(scope->values != NULL && ht_get(scope->values,&sym.integer,&r))
	 return r;
  return lisp_scope_get_value(scope->super, sym);
}

size_t ht_calc_hash(hash_table * ht, void * key);
bool ht_get_precalc(hash_table * ht, size_t hashed_key, const void *key, void * out_elem);

bool _lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out, size_t hash){
  if(scope == NULL) return false;
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(scope->lookup[i].car.type == LISP_NIL)
        break;
      if(scope->lookup[i].car.integer == sym.integer){
        *out = scope->lookup[i].cdr;
        return true;
      }
    }
  }
  if(scope->values != NULL && ht_get_precalc(scope->values, hash, &sym.integer, out))
	 return true;
  return _lisp_scope_try_get_value(scope->super, sym, out, hash);
}

bool lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out){
  if(scope == NULL) return false;
  if(scope->lookup != NULL){
    var argcnt = scope->argcnt;
    for(size_t i = 0; i < argcnt; i++){
      var v = scope->lookup[i].car.integer;
      if(v == 0) break;
      if(v == sym.integer){
        *out = scope->lookup[i].cdr;
        return true;
      }
    }
  }
  if(scope->values == NULL)
    return lisp_scope_try_get_value(scope->super, sym, out);
  size_t hash = ht_calc_hash(scope->values, &sym.integer);
  return _lisp_scope_try_get_value(scope, sym, out, hash);
}

lisp_value lisp_scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope == NULL){
	 raise_string("Variable not found");
	 return nil;
  }
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(scope->lookup[i].car.type == LISP_NIL)
        break;
      if(scope->lookup[i].car.integer == sym.integer){
        scope->lookup[i].cdr = value;
        return t;
      }
    }
  }
  if(scope->values != NULL && ht_get(scope->values,&sym.integer,NULL)){
	 ht_set(scope->values, &sym.integer, &value);
	 return t;
  }
  return lisp_scope_set_value(scope->super, sym, value);
}

lisp_value lisp_scope_create_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope->lookup != NULL){
    for(size_t i = 0; i < scope->argcnt; i++){
      if(scope->lookup[i].car.type == LISP_NIL){
        scope->lookup[i] = (cons){.car = sym, .cdr = value};
        return nil;
      }
    }
  }
  if(scope->values == NULL){
	 scope->values = ht_create(sizeof(u64), sizeof(lisp_value));
    //if(scope->super != NULL)
    //  raise(SIGINT);
  }
  
  ht_set(scope->values,&sym.integer,&value);
  return nil;
}

lisp_context * current_context;

lisp_context * lisp_context_new(){
  printf("LISP NEW CONTEXT\n");
  lisp_context * ctx = lisp_malloc(sizeof(ctx[0]));
  ctx->next_symbol = 1;
  ctx->symbols = ht_create_strkey(sizeof(u64));
  ctx->symbols_reverse = ht_create(sizeof(u64), sizeof(char *));
  ctx->globals = lisp_scope_new(NULL);
  var prev_ctx = current_context;
  current_context = ctx;
  rest_sym = get_symbol("&rest");
  if_sym = get_symbol("if");
  quote_sym = get_symbol("quote");
  quasiquote_sym = get_symbol("quasiquote");
  unquote_sym = get_symbol("unquote");
  unquote_splice_sym = get_symbol("unquote-splicing");
  lisp_scope_create_value(current_context->globals, get_symbol("nil"), nil);
  lisp_scope_create_value(current_context->globals, get_symbol("t"), t);
  current_context = prev_ctx;
  
  return ctx;
}

lisp_value car(lisp_value v){
  if(v.type == LISP_CONS)
	 return v.cons->car;
  return nil;
}

lisp_value cdr(lisp_value v){
  if(v.type == LISP_CONS)
	 return v.cons->cdr;
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
  var old = cons.cons->cdr;
  cons.cons->cdr = value;
  return old;
}

lisp_value set_car(lisp_value cons, lisp_value value){
  TYPE_ASSERT(cons, LISP_CONS);
  var old = cons.cons->car;
  cons.cons->car = value;
  return old;
}

lisp_value list_length(lisp_value lst){
  size_t l = 0;
  while(lst.type == LISP_CONS){
	 l += 1;
	 lst = cdr(lst);
  }
  return integer(l);
}

lisp_value pop(lisp_value * v){
  if(v->type != LISP_CONS)
	 return nil;
  lisp_value val = car(*v);
  *v= cdr(*v);
  return val;
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
  size_t c = 0;
  while(lst.type == LISP_CONS){
    c += 1;
    lst = lst.cons->cdr;
  }
  return integer(c);
}

size_t conses_allocated = 0;

lisp_value new_cons(lisp_value _car, lisp_value _cdr){
  lisp_value v = {.type = LISP_CONS, .cons = lisp_malloc(sizeof(cons))};
  v.cons->car = _car;
  v.cons->cdr = _cdr;
  conses_allocated += 1;
  return v;
}

lisp_value get_conses_allocated (){
  return integer(conses_allocated);
}

lisp_value copy_cons(lisp_value a){
  if(a.type == LISP_CONS)
    return new_cons(car(a), copy_cons(cdr(a)));
  return a;
}


lisp_value copy_cons_deep(lisp_value a){
  if(a.type == LISP_CONS)
    return new_cons(copy_cons(car(a)), copy_cons(cdr(a)));
  return a;
}

void skip_comment_and_whitespace(io_reader * rd){
  while(true){
	 uint8_t c = io_peek_u8(rd);
	 if(c == ' ' || c == '\n' || c == '\t'){
		io_read_u8(rd);
	 }
	 else if(c == ';'){
		while(true){
		  c = io_read_u8(rd);
		  if( c == '\n')
			 break;
		  if(c == 0)
			 break;
		}
	 }else{
		break;
	 }
  }
}

lisp_value read_token_string(io_reader * rd){
  
  io_writer wd = {0};
  io_read_u8(rd); // skip first quote.
  while(true){
	 uint8_t c = io_read_u8(rd);
    if(c == '\\'){
      var c2 = io_peek_u8(rd);
      if(c2 == 'n'){
        io_read_u8(rd);
        c = '\n';
      }else{
        c = io_read_u8(rd);
      }
    }else if(c == '"'){
		c = io_peek_u8(rd);
		if(c == '"'){
        io_read_u8(rd);
        //printf("double quote!\n");
		  io_write_u8(&wd, c);
		}else{
		  break;
		}
	 }
	 else  if(c == 0)
		break;// incomplete string.
	 io_write_u8(&wd, c);
  }
  io_write_u8(&wd, 0);
  lisp_value v = {
						.type = LISP_STRING,
						.string = gc_clone(wd.data, strlen(wd.data) + 1)
  };
  io_writer_clear(&wd);
  return v;
}

bool is_keyword(lisp_value sym){
  return sym.type == LISP_SYMBOL && sym.integer >= 0x100000;
}

int64_t get_symbol_id(const char * s){
  int64_t id = 0;
  if(ht_get(current_context->symbols, &s, &id))
    return id;
  
  s = gc_clone(s, strlen(s) + 1);
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
  return (lisp_value){.type = LISP_SYMBOL, .integer = get_symbol_id(s)};
}

const char * symbol_name(int64_t id){
  char * out;
  if(ht_get(current_context->symbols_reverse, &id, &out))
	 return out;
  return NULL;
}

lisp_value string_to_symbol(lisp_value string){
  TYPE_ASSERT(string, LISP_STRING);
  return get_symbol(string.string);
}

lisp_value symbol_to_string(lisp_value sym){
  TYPE_ASSERT(sym, LISP_SYMBOL);
  char * sym_string = (char *) symbol_name(sym.integer);
  return (lisp_value) {.type = LISP_STRING, .string = sym_string};
}

lisp_value string_starts_with(lisp_value str, lisp_value str2){
  TYPE_ASSERT(str, LISP_STRING);
  TYPE_ASSERT(str2, LISP_STRING);
  if(strncmp(str.string, str2.string, strlen(str2.string)) == 0)
    return t;
  return nil;
}


lisp_value parse_token(const char * x, int count){

  char * tp= NULL;

  {
	 int64_t o = strtoll(x, &tp, 10);
	 if(tp == x + count){
		return (lisp_value) {.type = LISP_INTEGER, .integer = o }; 
	 }
  }

  {
	 double o = strtold(x, &tp);
	 if(tp == x + count){
		return (lisp_value) {.type = LISP_RATIONAL, .rational = o }; 
	 }
  }
  
  // otherwise it is a symbol
  lisp_value r = {.type = LISP_SYMBOL, .integer = get_symbol_id(x)};

  return r;
}

lisp_value read_token_data(io_reader * rd){
  uint8_t c = io_peek_u8(rd);
  if(c == '"'){
	 return read_token_string(rd);
  }
  io_writer wd = {0};
  while(true){
	 c = io_peek_u8(rd);
	 if(c == ' ' || c == ')' || c == '(' || c == '\t' || c == 0 || c == '\n'){
		break;
	 }
	 io_read_u8(rd);
	 io_write_u8(&wd, c);
  }
  io_write_u8(&wd, 0);
  lisp_value vv = parse_token(gc_clone(wd.data, wd.offset), wd.offset - 1);
  io_writer_clear(&wd);
  return vv;
}

lisp_value tokenize_stream(io_reader * rd){
  skip_comment_and_whitespace(rd);
  uint8_t c = io_peek_u8(rd);
  if(c == 0) return nil;
  if(c == ':'){
    var symbol = read_token_data(rd);
    return symbol;
  }
  if(c == '\''){
	 io_read_u8(rd);
  	 var c = new_cons(quote_sym, new_cons(tokenize_stream(rd), nil));
    return c;
  }
  if(c == '`'){
	 io_read_u8(rd);
	 return new_cons(quasiquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == ','){
	 io_read_u8(rd);
	 if(io_peek_u8(rd) == '@'){
		io_read_u8(rd);
		return new_cons(unquote_splice_sym, new_cons(tokenize_stream(rd), nil));
	 }
	 return new_cons(unquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == '('){
	 io_read_u8(rd);

	 skip_comment_and_whitespace(rd);
	 if(io_peek_u8(rd) == ')'){
		io_read_u8(rd);
		return nil;
	 }
	 lisp_value head = nil;
	 var next = head;
	 while(true){
		var v = tokenize_stream(rd);
		var new = new_cons(v, nil);
		if(head.type == LISP_NIL){
		  head = new;
		}else{
		  next.cons->cdr = new;
		}
		next = new;
		skip_comment_and_whitespace(rd);
		uint8_t c = io_peek_u8(rd);
		if(c == 0 || c == ')'){
		  io_read_u8(rd);
		  break;
		}
		if(c == '.'){
		  var save = *rd;
		  io_read_u8(rd);
		  var loc = io_getloc(rd);
		  skip_comment_and_whitespace(rd);
		  var nextloc = io_getloc(rd);
		  if(loc == nextloc){
			 *rd = save;
		  }else{
			 var v = tokenize_stream(rd);
			 next.cons->cdr = v;
			 skip_comment_and_whitespace(rd);
			 if(io_read_u8(rd) != ')'){
				raise_string("Unexpected token");
			 }
			 break;
		  }

		}
	 }
	 return head;
	 
  }else{
	 skip_comment_and_whitespace(rd);
	 return read_token_data(rd);
  }
}


lisp_value lisp_read_stream(io_reader * rd){
  	 return tokenize_stream(rd);
}

lisp_value lisp_read_string(const char * str){

  io_reader w = io_reader_from_bytes((void *)str, strlen(str) + 1);
  w.offset = 0;
  return lisp_read_stream(&w);
}

lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value);
bool lisp_value_eq(lisp_value a, lisp_value b){
  if(a.type != b.type) return false;
  return a.integer == b.integer;
}
lisp_value lisp_sub_macro_expand(lisp_scope * scope, lisp_value c){
  if(c.type == LISP_NIL) return nil;
  var current = car(c);
  var next = cdr(c);
  var value = lisp_macro_expand(scope, current);
  if(next.type != LISP_NIL){
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
  if(value.type != LISP_CONS)
	 return value;
  if(lisp_value_eq(car(value), quote_sym)) return value;
  var value_new = lisp_sub_macro_expand(scope, value);
  if(!lisp_value_eq(value_new, value))
	 return value_new;
  lisp_value head = car(value);
  if(head.type != LISP_SYMBOL) return value;
  if(head.symbol == quote_sym.symbol) return value;
  lisp_value head_value = lisp_scope_get_value(scope, head);
  if(head_value.type != LISP_FUNCTION_MACRO) return value;
  lisp_function * f = head_value.function;
  let argcnt = lisp_length(f->args).integer;
  cons args3[argcnt];
  memset(args3, 0, sizeof(args3[0]) * argcnt);
  lisp_scope function_scope[1] = {0};
  lisp_scope_stack(function_scope, f->closure, args3, argcnt);
  var args = f->args;
  var args2 = cdr(value);
  while(args.type != LISP_NIL){
	 var arg = car(args);
	 var argv = car(args2);
	 
	 if(arg.type != LISP_SYMBOL){
		println(value);
		println(args);
		println(args2);
		raise_string("arg name must be a symbol\n");
		return nil;
	 }
	 if(arg.symbol == rest_sym.symbol){
		args = cdr(args);
		arg = car(args);
		if(arg.type != LISP_SYMBOL){
		  raise_string("(2) arg name must be a symbol.");
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
  while(it.type != LISP_NIL){
	 ret = lisp_eval(function_scope, car(it));
	 it = cdr(it);
  }
		  
  return ret;
}


lisp_value lisp_sub_eval(lisp_scope * scope, lisp_value c, cons * cons_buf){
  if(c.type == LISP_NIL) return nil;
  var next = cdr(c);
  if(next.type != LISP_NIL){

	 var value = lisp_eval(scope, car(c));
	 var nextr = lisp_sub_eval(scope, next, cons_buf + 1);
    lisp_value cns = (lisp_value){.cons = cons_buf, .type = LISP_CONS};
	 cns.cons->car =value;
    cns.cons->cdr = nextr;
    return cns;
  }else{
    lisp_value cns = (lisp_value){.cons = cons_buf, .type = LISP_CONS};
    cns.cons->car = lisp_eval(scope, car(c));
    cns.cons->cdr = nil;
    return cns;
  }
}
lisp_value lisp_eval_quasiquoted(lisp_scope * scope, lisp_value value);

lisp_value lisp_eval_quasiquoted_sub(lisp_scope * scope, lisp_value value){
  if(is_nil(value)) return value;
  var current = car(value);
  var next = cdr(value);
  bool unsplice = _lisp_eq(car(current), unquote_splice_sym);
  var value2 = lisp_eval_quasiquoted(scope, current);
  if(next.type != LISP_NIL){
	 var nextr = lisp_eval_quasiquoted_sub(scope, next);
	 if(lisp_value_eq(current, value2) && lisp_value_eq(next, nextr))
		return value;
	 if(unsplice){
		return lisp_append(value2, nextr);
	 }
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
  switch(value.type){
  case LISP_CONS:
	 {
		var fst = car(value);
      
		if(_lisp_eq(fst, unquote_sym))
		  return lisp_eval(scope, cadr(value));
		if(_lisp_eq(fst, unquote_splice_sym)){
		  var to_splice = lisp_eval(scope, cadr(value));
		  return to_splice;
		}
		  
		return lisp_eval_quasiquoted_sub(scope, value);
	 }
  default:
	 return value;
  }
}

static __thread lisp_value call_chain = {0};
void print_call_stack(){
  lisp_value top = call_chain;
  int id = 1;
  printf("Stack trace:\n");
  while(top.type == LISP_CONS){
    printf(" %i - ", id++);
    println(car(top));
    top = cdr(top);
  }
}

lisp_value lisp_eval2(lisp_scope * scope, lisp_value value);
lisp_value lisp_eval(lisp_scope * scope, lisp_value value){
  if(lisp_is_in_error())
    return nil;
  cons c = {.car = value, .cdr = call_chain};
  call_chain = (lisp_value){.cons = &c, .type = LISP_CONS};
  var r = lisp_eval2(scope, value);
  call_chain = c.cdr;
  return r;
}

lisp_value lisp_eval2(lisp_scope * scope, lisp_value value){

  switch(value.type){
  case LISP_CONS:
	 {
      var first = car(value);
      //println(first);
      if(first.type == LISP_NIL){
        raise_string("called with nil");
        return nil;
      }

      lisp_value first_value = lisp_eval(scope, first);
		if(first_value.type == LISP_MACRO_BUILTIN){
		  switch(first_value.builtin){
		  case LISP_IF:
			 {

				var cond = lisp_eval(scope, cadr(value));
				if(cond.type == LISP_NIL){
				  return lisp_eval(scope, cadddr(value));
				}else{
				  return lisp_eval(scope, caddr(value));
				}
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
            var argcnt = lisp_length(argform).integer;
            cons argsbuf[argcnt];

            memset(argsbuf, 0, sizeof(argsbuf[0]) * argcnt);
            lisp_scope scope1[1] = {0};
            lisp_scope_stack(scope1, scope, argsbuf, argcnt);
            scope = scope1;
            while(argform.type != LISP_NIL){
				  var arg = car(argform);
				  var sym = car(arg);
				  var value = lisp_eval(scope, cadr(arg));
				  lisp_scope_create_value(scope, sym, value);
				  argform = cdr(argform);
				}
				value = cdr(value);
            var body = cdr(value);
				lisp_value result = nil;
				while(body.type != LISP_NIL){
				  result = lisp_eval(scope, car(body));
				  body = cdr(body);
				}
				return result;
			 }
		  case LISP_PROGN:
			 {
				var body = cdr(value);
				lisp_value result = nil;
				while(body.type != LISP_NIL){
				  result = lisp_eval(scope, car(body));
				  body = cdr(body);
				}
				return result;
			 }
		  case LISP_LOOP:
			 {
				var cond = cadr(value);
				var _body = cddr(value);
				lisp_value result = nil;
				while(lisp_eval(scope, cond).type != LISP_NIL){
				  var body = _body;;
				  while(body.type != LISP_NIL){
					 result = lisp_eval(scope, car(body));
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
				var sym = cadr(value);
				if(sym.type != LISP_SYMBOL){
				  return nil; // error
				}
				var value2 = lisp_eval(scope, caddr(value));
				lisp_scope_set_value(scope, sym, value2);
				return value2;
			 }
		  case LISP_DEFINE:
			 {
				var sym = cadr(value);
				if(sym.type != LISP_SYMBOL)
				  return nil; // error
				
				var value2 = lisp_eval(scope, caddr(value));
				lisp_scope_create_value(scope, sym, value2);
				return value2;
			 }
		  case LISP_EVAL:
			 {
				return lisp_eval(scope, lisp_eval(scope, cadr(value)));
			 }
        case LISP_SYMBOL_VALUE:
          {
            var sym = lisp_eval(scope, cadr(value));
            
            if(sym.type != LISP_SYMBOL){
              println(sym);
              printf("Not a symbol\n");
              return nil;
            }
            if(caddr(value).type != LISP_NIL)
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
            if(sym.type != LISP_SYMBOL)
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
        }
      }
      size_t argcnt = lisp_length(cdr(value)).integer;
      cons args[argcnt];
		lisp_value things = lisp_sub_eval(scope, cdr(value), args);
      argcnt += 1;
      if(lisp_is_in_error())
        return nil;

		if(first_value.type == LISP_FUNCTION_NATIVE){
      
		  var n = first_value.nfunction;
        if(n->fptr == NULL){
          raise_string("function is null");
          return nil;
        }
		  union{
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
		  case 0:
			 return f.n0();
		  case 1:
			 return f.n1(args[0].car);
		  case 2:
			 return f.n2(args[0].car, args[1].car);
        case 3:
			 return f.n3(args[0].car, args[1].car, args[2].car);       
		  case 4:
			 return f.n4(args[0].car, args[1].car, args[2].car, args[3].car);
		  case 5:
			 return f.n5(args[0].car, args[1].car, args[2].car, args[3].car, args[4].car);
          
		  case 6:
			 return f.n6(args[0].car, args[1].car, args[2].car, args[3].car, args[4].car, args[5].car);
        default:
			 raise_string("Unsupported number of args");
        }
		}else if(first_value.type == LISP_FUNCTION){
		  
		  var f = first_value.function;
        cons args3[argcnt];
        memset(args3, 0, sizeof(args3[0]) * (argcnt));
        //cons * args3 = NULL;
        /*if(argcnt > 0){
          args3 = lisp_malloc(argcnt * sizeof(cons));
          memset(args3, 0, sizeof(cons) * argcnt);
          }*/
        
		  lisp_scope function_scope[1];
        
        lisp_scope_stack(function_scope, f->closure,  args3, argcnt);
		  var args = f->args;
		  var args2 = things;

		  while(args.type != LISP_NIL){
			 var arg = car(args);
			 if(arg.type != LISP_SYMBOL){
				println(f->args);
				raise_string("(3) arg name must be a symbol");
				return nil;
			 }
			 if(arg.symbol == rest_sym.symbol){
				args = cdr(args);
				arg = car(args);
				if(arg.type != LISP_SYMBOL){
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
		  while(it.type != LISP_NIL){
			 ret = lisp_eval(function_scope, car(it));
			 it = cdr(it);
		  }
		  
		  return ret;
		}else if(first_value.type == LISP_ALIEN_FUNCTION){
		  var func = first_value.alien_func;
		  var arg_len = lisp_len(func->arg_example).integer;
		  
		  union{
			 int64_t (* n0)();
			 int64_t (* n1)(int64_t);
			 int64_t (* n2)(int64_t, int64_t);
          double (* n1d)(double);
			 double (* n2d)(double, double);
			 int64_t (* n3)(int64_t, int64_t, int64_t);
			 int64_t (* n4)(int64_t, int64_t, int64_t, int64_t);
			 int64_t (* n5)(int64_t, int64_t, int64_t, int64_t, int64_t);
			 int64_t (* n6)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t);
			 void (* nr0)();
			 void (* nr1)(int64_t);
			 void (* nr2)(int64_t, int64_t);
			 void (* nr3)(int64_t, int64_t, int64_t);
			 void (* nr4)(int64_t, int64_t, int64_t, int64_t);
			 void (* nr5)(int64_t, int64_t, int64_t, int64_t, int64_t);
			 void (* nr6)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t);
			
		  }f;
		  f.n0 = func->func;
		  
		  i64 r = 0;
		  if(func->return_example.type != LISP_NIL){
			 switch(arg_len){
			 case 0:
				r = f.n0();
				break;
			 case 1:
            if(car(things).type == LISP_RATIONAL)
              r = f.n1d(car(things).rational);
            else
              r = f.n1(car(things).integer);
				break;
			 case 2:
            if(car(things).type == LISP_RATIONAL)
              r = f.n2d(car(things).rational, cadr(things).rational);
            else
              r = f.n2(car(things).integer, cadr(things).integer);
				break;
			 case 3:
				r = f.n3(car(things).integer, cadr(things).integer, caddr(things).integer);
				break;
			 case 4:
				r = f.n4(car(things).integer, cadr(things).integer, caddr(things).integer, cadddr(things).integer);
				break;
			 default:
				
				printf("invalid argnr for "); print(value); printf("\n");
				raise_string("");
			 }
		  }else{
			 switch(arg_len){
			 case 0:
				f.nr0();
				break;
			 case 1:
				f.nr1(car(things).integer);
				break;
			 case 2:
				f.nr2(car(things).integer, cadr(things).integer);
				break;
			 case 3:
				f.nr3(car(things).integer, cadr(things).integer, caddr(things).integer);
				break;
			 case 4:
				f.nr4(car(things).integer, cadr(things).integer, caddr(things).integer, cadddr(things).integer);
				break;
			 default:
				raise_string("unsupported number of arguments\n");
			 }
		  }
		  var ret = func->return_example;
		  ret.integer = r;
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
		 
  default:
	 return value;
  }
  return nil;
}

lisp_value lisp_eval_value(lisp_value code){
  while(true){
    var next_code = lisp_macro_expand(current_context->globals, code);
		
    if(lisp_value_eq(next_code, code))
      break;
    code = next_code;
  }
  //println(code);
  return lisp_eval(current_context->globals, code);
}

lisp_value lisp_eval_stream(io_reader * rd){
  lisp_value result = nil;
  while(true){
	 var off = rd->offset;
	 var code = lisp_read_stream(rd);
	 if(off == rd->offset || code.type == LISP_NIL) break;
	 while(true){
      if(lisp_is_in_error())
        return nil;
		var next_code = lisp_macro_expand(current_context->globals, code);
		
		if(lisp_value_eq(next_code, code))
		  break;
		code = next_code;
	 }
	 println(code);

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
  FILE * f = fopen(filepath, "r");
  if(f == NULL) return nil;
  fseek(f,0,SEEK_END);
  size_t size = ftell(f);
  char * buffer = malloc(size + 1);
  buffer[size] = 0;
  fseek(f, 0, SEEK_SET);
  size_t l = fread(buffer, size,1,f);
  (void)(l);
  
  fclose(f);
  lisp_value r = lisp_eval_string(buffer);
  free(buffer);
  return r;
}

int print2(char * buffer, int l2, lisp_value v){
  char * initbuf = buffer;
  int l = 0;
#define OBUF buffer ? (initbuf + l) : buffer
#define LEN1 l2 ? (l2 - l) : 0

  switch(v.type){
  case LISP_NIL:
	 return snprintf(buffer, LEN1, "()");
  case LISP_T:
	 return snprintf(buffer, LEN1, "t");
  case LISP_INTEGER:
	 return snprintf(buffer, LEN1, "%lli", v.integer);
  case LISP_BYTE:
	 return snprintf(buffer, LEN1, "%i", (u8)v.integer);
  case LISP_VECTOR:
	 {
		int l = 0;
      l = snprintf(buffer, LEN1, "#(");
      
		var vector = v.vector;
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
  case LISP_ALIEN_FUNCTION:
	 return snprintf(buffer, LEN1, "ALIENF %p", v.alien_func->func);
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
  case LISP_CONS:
    {
      int l = 0;
      l = snprintf(buffer, LEN1, "(");
      var first = true;
      while(v.type == LISP_CONS){
        if(first){
          first = false;
        }else{
          l += snprintf(OBUF, LEN1, " ");
        }
        l += print2(OBUF, LEN1, v.cons->car);
        v = v.cons->cdr;
      }
      if(v.type != LISP_NIL){
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
  int l = print2(NULL,0,  v);
  char * str = lisp_malloc(l + 1);
  print2(str, l+ 1, v);
  printf("%s", str);
  return integer(l);
}

lisp_value value_to_string(lisp_value v){
  int l = print2(NULL,0,  v);
  char * str = lisp_malloc(l+ 1);
  print2(str, l + 1, v);
  return (lisp_value){.type = LISP_STRING, .string = str};
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

lisp_value lisp_add(lisp_value a, lisp_value b){
  if(is_float(a))
	 a.rational += b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer += b.integer;
  return a;
}

lisp_value lisp_sub(lisp_value a, lisp_value b){
  if(is_float(a))
	 a.rational -= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer -= b.integer;
  return a;
}

lisp_value lisp_mul(lisp_value a, lisp_value b){
  if(is_float(a))
	 a.rational *= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer *= b.integer;
  return a;
}
lisp_value lisp_div(lisp_value a, lisp_value b){
  if(is_float(a))
	 a.rational /= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer /= b.integer;
  return a;
}

lisp_value lisp_less(lisp_value a, lisp_value b){
  if(is_float(a) && a.rational < b.rational)
	 return t;
  if(a.type == LISP_INTEGER && a.integer < b.integer)
	 return t;
  return nil;
}
lisp_value lisp_greater(lisp_value a, lisp_value b){
  if(is_float(a) && a.rational > b.rational)
	 return t;
  if(a.type == LISP_INTEGER && a.integer > b.integer)
	 return t;
  return nil;
}

lisp_value lisp_eq(lisp_value a, lisp_value b){
  return _lisp_eq(a,b) ? t : nil;
}

bool eq(lisp_value a, lisp_value b){
  return _lisp_eq(a, b);
}

lisp_value lisp_len(lisp_value a){
  int64_t i = 0;
  while(a.type == LISP_CONS){
	 i += 1;
	 a = a.cons->cdr;
  }
  return (lisp_value){.type = LISP_INTEGER, .integer = i};
}

lisp_value println(lisp_value v){
  print(v);
  printf("\n");
  return v;
}

lisp_value lisp_integer(lisp_value v){
  if(v.type == LISP_RATIONAL)
	 v.integer = (int64_t) v.rational;
  v.type = LISP_INTEGER;
  return v;
}

lisp_value lisp_byte(lisp_value v){
  v = lisp_integer(v);
  v.integer = (u8)v.integer;
  v.type = LISP_BYTE;
  return v;
}


lisp_value lisp_rational(lisp_value v){
  if(v.type != LISP_RATIONAL && v.type != LISP_FLOAT32)
	 v.rational = (double) v.integer;
  v.type = LISP_RATIONAL;
  return v;
}

lisp_value rational(double v){
  return (lisp_value){.rational = v, .type = LISP_RATIONAL};
}
lisp_value float32(float v){
  return (lisp_value){.rational = v, .type = LISP_FLOAT32};
}

lisp_value lisp_float32(lisp_value v){
  if(v.type != LISP_RATIONAL && v.type != LISP_FLOAT32)
	 v.rational = (double) v.integer;
  v.type = LISP_FLOAT32;
  return v;
}

lisp_value lisp_panic(lisp_value v){
  return lisp_error(v);
}

lisp_value integer(int64_t v){
  return (lisp_value){.type = LISP_INTEGER, .integer = v};
}

lisp_value byte(unsigned char v){
  return (lisp_value){.type = LISP_BYTE, .integer = v};
}


lisp_value native_pointer(void * ptr){
  return (lisp_value){.type = LISP_NATIVE_POINTER, .native_pointer = ptr};
  
}

lisp_value gc_heap(){
  return integer(GC_get_heap_size());
}

lisp_value lisp_sin(lisp_value v){
  var p = lisp_rational(v);
  p.rational = sin(p.rational);
  return p;
}

lisp_value lisp_read(lisp_value v){
  if(v.type != LISP_STRING)
	 return v;
  return lisp_read_string(v.string);
}

size_t alloc_total = 0;
void * lisp_malloc(size_t v){
  alloc_total += v;
  //printf("allocate %i / %i kB\n", v, alloc_total / 1000);
  //if((alloc_total & 0xFFFFF) == 0xFFFFF){
    //printf("allocate %i / %i kB\n", v, alloc_total / 1000);
    //raise(SIGINT);
    //}
  return calloc(v, 1);
  //return GC_malloc(v);
}

void * lisp_malloc_atomic(size_t v){
  return lisp_malloc(v);
  //return GC_malloc_atomic(v);
}

void lisp_free(void * p){
  free(p);
}

const char * lisp_type_to_string(lisp_type t){
  switch(t){
  case LISP_NIL: return "NIL";
  case LISP_T: return "T";
  case LISP_CONS: return "CONS";
  case LISP_INTEGER: return "INTEGER";
  case LISP_RATIONAL: return "RATIONAL";
  case LISP_STRING: return "STRING";
  case LISP_SYMBOL: return "SYMBOL";
  case LISP_FUNCTION: return "FUNCTION";
  case LISP_FUNCTION_MACRO: return "MACRO";
  case LISP_FUNCTION_NATIVE: return "NATIVE_FUNCTION";
  case LISP_MACRO_BUILTIN: return "MACRO_BUILTIN";
  case LISP_NATIVE_POINTER: return "NATIVE_POINTER";
  case LISP_ALIEN_FUNCTION: return "ALIEN_FUNCTION";
  case LISP_VECTOR: return "VECTOR";
  case LISP_BYTE: return "BYTE";
  case LISP_FLOAT32: return "FLOAT32";
  }
  raise_string("Unknown type:\n");
  
  return NULL;
}

lisp_value lisp_type_of(lisp_value v){
  return get_symbol(lisp_type_to_string(v.type));
}

lisp_value lisp_load(lisp_value v){
  TYPE_ASSERT(v, LISP_STRING);
  return lisp_eval_file(v.string);
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
  size_t l = (size_t)len.integer;
  size_t elem_size = lisp_type_size(_default.type);
  void * data = lisp_malloc(l * elem_size);

  lisp_vector * vector = lisp_malloc(sizeof(*vector));
  vector->data = data;
  vector->count = l;
  vector->elem_size = elem_size;
  vector->default_value = _default;
  lisp_value v = {.type = LISP_VECTOR, .vector = vector};
  for(size_t i = 0; i < l; i++)
    vector_set(v, integer(i),_default);
    
  return v;
}

lisp_value vector_length(lisp_value v){
  TYPE_ASSERT(v, LISP_VECTOR);
  return integer(v.vector->count);
}

lisp_value vector_ref(lisp_value _vector, lisp_value k){
  TYPE_ASSERT(_vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var vector = _vector.vector;
  var v = vector->default_value;
  void * src = vector->data + k.integer * vector->elem_size;
  void * dst;
  if(v.type == LISP_NIL){
	 return ((lisp_value *) vector->data)[k.integer];
  }else if(v.type == LISP_FLOAT32){
	 v.rational = ((float *) vector->data)[k.integer];
	 return v;
  }else
	 dst = &v.integer;
  memcpy(dst, src, vector->elem_size);
  
  return v;
}
lisp_value vector_set(lisp_value _vector, lisp_value k, lisp_value value){
  TYPE_ASSERT(_vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  
  var vector = _vector.vector;
  var v = value;
  void * dst = vector->data + k.integer * vector->elem_size;
  if(k.integer >= vector->count){
    raise_string("index outside of the bounds of the vector\n");
    return nil;
  }
  void * src;
  if(vector->default_value.type == LISP_NIL)
	 ((lisp_value *) vector->data)[k.integer] = value;
  else{
	 TYPE_ASSERT(value, vector->default_value.type);
	 src = &v.integer;
	 if(vector->default_value.type == LISP_FLOAT32){
		((float *) src)[0] = v.rational;
	 }
	
	 memcpy(dst, src, vector->elem_size);
  }
  return nil;
}

lisp_value vector_resize(lisp_value vector, lisp_value k){
  TYPE_ASSERT(vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);

  size_t l = (size_t)k.integer;
  size_t elem_size = lisp_type_size(vector.vector->default_value.type);
  
  void * new_data = lisp_malloc(l * elem_size);
  size_t prevCount = MIN(l, vector.vector->count);   
  memcpy(new_data, vector.vector->data, prevCount * elem_size);
  vector.vector->data = GC_realloc(vector.vector->data, l * elem_size);
  vector.vector->count = l;
  for(size_t i = prevCount; i < l; i++){
	 vector_set(vector, integer(i), vector.vector->default_value);
  }

  return vector;
}

lisp_value vector_elem_type(lisp_value vector){
  TYPE_ASSERT(vector, LISP_VECTOR);
  return lisp_type_of(vector.vector->default_value);
}

lisp_value vector_copy(lisp_value vector){
  TYPE_ASSERT(vector, LISP_VECTOR);
  let vector2 = make_vector(integer(vector.vector->count), vector.vector->default_value);
  memcpy(vector2.vector->data, vector.vector->data, vector2.vector->count * vector2.vector->elem_size);
  return vector2;
}

lisp_value vector_to_string(lisp_value vector){
  vector = vector_copy(vector);
  vector.vector->count = vector.vector->count * vector.vector->elem_size;
  vector.vector->default_value = byte(0);
  var cnt = vector.vector->count;
  u8 * ptr = vector.vector->data;
  size_t i;
  for(i = 0; i < cnt; i++){
    if(ptr[i] == 0) break;
  }
  if(i == cnt)
    i = i + 1;
  vector = vector_resize(vector, integer(i));

  lisp_value str = {.type = LISP_STRING, .string = vector.vector->data};
  return str;
}

lisp_value string_to_vector(lisp_value str){
  TYPE_ASSERT(str, LISP_STRING);
  char * strbuf = str.string;
  size_t l = strlen(strbuf) + 1;
  size_t elem_size = 1;

  lisp_vector * vector = lisp_malloc(sizeof(*vector));
  vector->data = strbuf;
  vector->count = l - 1;
  vector->elem_size = elem_size;
  vector->default_value = byte(0);
  lisp_value v = {.type = LISP_VECTOR, .vector = vector};
  return v;
}

lisp_value vector_native_element_pointer(lisp_value vector, lisp_value k){
  TYPE_ASSERT(vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  
  void * ptr = vector.vector->data + k.integer * vector.vector->elem_size;
  
  return (lisp_value){.type = LISP_NATIVE_POINTER, .native_pointer = ptr};
}


lisp_value parse_hex(lisp_value str){
  TYPE_ASSERT(str, LISP_STRING);
  char * tp = NULL;
  int64_t o = strtoll(str.string, &tp, 16);
  if(tp != str.string )
    return (lisp_value) {.type = LISP_INTEGER, .integer = o }; 
  return nil;
}

lisp_value hex_string(lisp_value i, lisp_value dec){
  int v = i.integer;
  int l = dec.integer;
  int cnt = snprintf(NULL, 0, "%x", v);
  if(is_nil(dec))
    l = cnt;
  char * buf = lisp_malloc(l + 1);
  printf("%i %i %i\n", v, l, cnt);
  snprintf(buf + MAX(0, l - cnt), MIN(cnt, l) + 1, "%x", v);
  buf[l] = 0;
  for(int i = 0; i < MAX(0, l - cnt); i++)
    buf[i] = '0';
  return (lisp_value){.type = LISP_STRING, .string = buf};
}

lisp_value lisp_signature(lisp_value func){
  if(func.type == LISP_FUNCTION)
    return new_cons(get_symbol("func"), func.function->args);
  return nil;
}

void item_finalizer(void * obj, void * data){
  lisp_value item = {.type = LISP_CONS, .cons = obj};
  lisp_value func = {.type = LISP_FUNCTION, .function = data};
  lisp_value eval = new_cons(func, new_cons(new_cons(quote_sym, new_cons(item, nil)), nil));
  
  lisp_eval(current_context->globals, eval);
}

lisp_value lisp_register_finalizer(lisp_value item, lisp_value func){
  return nil;
  TYPE_ASSERT(item, LISP_CONS);
  TYPE_ASSERT(func, LISP_FUNCTION);
  GC_REGISTER_FINALIZER(item.cons, item_finalizer, func.function, 0, 0);
  return nil;
}

lisp_value lisp_make_hashtable(lisp_value weak_key, lisp_value weak_value){
    bool weak_keys = !is_nil(weak_key);
  bool weak_values = !is_nil(weak_value);
  hash_table * ht = ht_create(sizeof(lisp_value), sizeof(lisp_value));
  if(weak_keys)
	 ht_set_mem_keys(ht, lisp_malloc_atomic, lisp_free);
  if(weak_values)
  ht_set_mem_values(ht, lisp_malloc_atomic, lisp_free);
  return native_pointer(ht);
}

lisp_value lisp_hashtable_set(lisp_value _ht, lisp_value key, lisp_value value){
  TYPE_ASSERT(_ht, LISP_NATIVE_POINTER);
  hash_table * ht = _ht.native_pointer;
  ht_set(ht, &key, &value);
  return nil;
}

lisp_value lisp_hashtable_get(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_NATIVE_POINTER);
  hash_table * ht = _ht.native_pointer;
  lisp_value value;
  if(ht_get(ht, &key, &value))
	 return value;
  return nil;
}

void it_symbols(void * k1, void * k2, void * target){
  lisp_value * val = target;
  const int64_t * sym = k2;
  lisp_value prev = *val;
  *val = new_cons((lisp_value){.type = LISP_SYMBOL, .integer = *sym}, prev); 
}

lisp_value lisp_all_symbols(){
  lisp_value val = nil;
  hash_table * symbols = current_context->symbols;
  ht_iterate(symbols, it_symbols, &val);
  return val;
}

lisp_value lisp_hashtable_remove(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_NATIVE_POINTER);
  hash_table * ht = _ht.native_pointer;
  if(ht_remove(ht, &key))
	 return t;
  return nil;
}

lisp_value lisp_hashtable_get2(lisp_value _ht, lisp_value key){
  TYPE_ASSERT(_ht, LISP_NATIVE_POINTER);
  hash_table * ht = _ht.native_pointer;
  lisp_value value;
  if(ht_get(ht, &key, &value))
	 return new_cons(t, value);
  return nil;
}

lisp_value lisp_is_symbol(lisp_value v){
  if(v.type == LISP_SYMBOL)
    return t;
  return nil;
}

lisp_value lisp_plookup(lisp_value lst, lisp_value sym){
  while(lst.type == LISP_CONS){
    if(car(lst).type == LISP_SYMBOL){
      if(sym.integer == car(lst).integer)
        return cadr(lst);
      lst = cddr(lst);
    }else{
      lst = cdr(lst);
    }

  }
  return nil;
}

lisp_value lisp_mapn(lisp_value f, lisp_value lst){
  var glob =  current_context->globals;
  while(lst.type == LISP_CONS){
    var v = car(lst);
    cons e2 = {.car = v, .cdr = nil};
    lisp_value ev2 = {.type = LISP_CONS, .cons = &e2};
    cons e1 = {.car = f, .cdr = ev2};
    lisp_value ev1 = {.type = LISP_CONS, .cons = &e1};
    lisp_eval(glob, ev1);
    lst = cdr(lst);

  }
  return nil;
}
  

lisp_value lisp_exit(){
  exit(0);
  return nil;
}
void foxgl_register();

void load_modules(){
  foxgl_register();

}


void web_update(){
  debug_set = true;
  var sym = get_symbol("lisp:*web-update*");
  debug_set = false;
  println(sym);
  lisp_eval(current_context->globals, new_cons(sym, nil));
  //lisp_eval_string("(lisp:*web-update*)");
  printf("Web update\n");
}
void warn_gc(char * msg, GC_word arg){
  printf("%s %i\n", msg, arg);
}
//void setup_signal_handler();
int main(int argc, char ** argv){
  //GC_no_dls = 1;
  GC_set_warn_proc(warn_gc);
  GC_INIT();
  printf("starting..\n");
  //setup_signal_handler();
  ht_mem_malloc = lisp_malloc;
  ht_mem_free = lisp_free;
  current_context = lisp_context_new();
  lisp_register_native("gc-heap", 0, gc_heap);
  lisp_register_native("lisp:exit", 0, lisp_exit);
  lisp_register_native("+", 2, lisp_add);
  lisp_register_native("symbol?", 1, lisp_is_symbol);
  lisp_register_native("-", 2, lisp_sub);
  lisp_register_native("*", 2, lisp_mul);
  lisp_register_native("/", 2, lisp_div);
  lisp_register_native("<", 2, lisp_less);
  lisp_register_native(">", 2, lisp_greater);
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
  lisp_register_native("length", 1, list_length);
  lisp_register_native("lisp:get-conses-allocated", 0, get_conses_allocated);
  lisp_register_native("copy-list", 1, copy_cons);
  lisp_register_native("copy-list-deep", 1, copy_cons);
  lisp_register_native("plookup", 2, lisp_plookup);
  //lisp_register_native("map!", 2, lisp_mapn);

  lisp_register_native("=", 2, lisp_eq);
  lisp_register_native("string=", 2, string_eq);
  lisp_register_native("panic", 1, lisp_error);
  lisp_register_native("integer", 1, lisp_integer);
  lisp_register_native("rational", 1, lisp_rational);
  lisp_register_native("float32", 1, lisp_float32);
  lisp_register_native("byte", 1, lisp_byte);
  lisp_register_native("sin", 1, lisp_sin);
  lisp_register_native("type-of", 1, lisp_type_of);
  
  lisp_register_native("read-string", 1, lisp_read);
  lisp_register_native("panic", 1, lisp_panic);
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
  
  lisp_register_native("register-finalizer", 2, lisp_register_finalizer);
  
  lisp_register_native("eval", 1, lisp_eval_value);
  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  lisp_register_macro("let", LISP_LET);
  lisp_register_macro("progn", LISP_PROGN);
  lisp_register_macro("lambda", LISP_LAMBDA);
  lisp_register_macro("loop", LISP_LOOP);
  lisp_register_macro("macro", LISP_MACRO);
  lisp_register_macro("set", LISP_SET);
  lisp_register_macro("def", LISP_DEFINE);
  //lisp_register_macro("eval", LISP_EVAL);
  lisp_register_macro("quasiquote", LISP_QUASIQUOTE);
  lisp_register_macro("unquote", LISP_UNQUOTE);
  lisp_register_macro("symbol-value", LISP_SYMBOL_VALUE);
  lisp_register_macro("bound?", LISP_BOUND);
  lisp_register_macro("with-exception-handler", LISP_WITH_EXCEPTION_HANDLER);

  lisp_register_value("native-null-pointer", (lisp_value){.type = LISP_NATIVE_POINTER, .integer = 0});
#ifndef WASM
  GC_allow_register_threads();
  lisp_register_value("lisp:*web-environment*", nil);
#else
  lisp_register_value("lisp:*web-environment*", t);
  lisp_register_value("lisp:*web-update*", nil);

#endif

  load_modules();
  lisp_eval_file("ld50.lisp");
#ifdef WASM
  emscripten_set_main_loop(web_update, 0, 1);
#endif
  return 0;
}

pthread_t foxlisp_create_thread(void * (* f)(void * data), void * data){
  pthread_t thread;
  GC_pthread_create(&thread, NULL, f, data);
  return thread;
}
