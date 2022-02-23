#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>
#include <gc.h>
#include <dlfcn.h>
#include "foxlisp.h"
void ht_free(hash_table *ht);
extern void * (* ht_mem_malloc)(size_t s);
extern void (* ht_mem_free)(void *);

void error(){
    raise(SIGINT);
}

bool is_nil(lisp_value v){
  return v.type == LISP_NIL;
}

bool  _lisp_eq(lisp_value a, lisp_value b){
  if(a.type != b.type) return false;
  if(a.type == LISP_RATIONAL) return a.rational == b.rational;
  return a.integer == b.integer;
}

void * gc_clone(void * mem, size_t s){
  void * d = GC_MALLOC(s);
  memcpy(d, mem, s);
  return d;
}

lisp_scope * scope_new(lisp_scope * super){
  lisp_scope * s = GC_MALLOC(sizeof(*super));
  s->super = super;
  s->values = NULL;
  return s;
}

lisp_value scope_get_value(lisp_scope * scope, lisp_value sym){
  if(scope == NULL) return nil;
  lisp_value r;
  if(scope->values != NULL && ht_get(scope->values,&sym.integer,&r))
	 return r;
  return scope_get_value(scope->super, sym);
}

bool scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out){
  if(scope == NULL) return false;
  if(scope->values != NULL && ht_get(scope->values,&sym.integer, out))
	 return true;
  return scope_try_get_value(scope->super, sym, out);
}

lisp_value scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope == NULL){
	 printf("Error: variable '%s' not found\n", symbol_name(sym.integer));
	 error();
	 return nil;
  }
  if(scope->values != NULL && ht_get(scope->values,&sym.integer,NULL)){
	 ht_set(scope->values, &sym.integer, &value);
	 return t;
  }
  return scope_set_value(scope->super, sym, value);
}

lisp_value scope_create_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope->values == NULL)
	 scope->values = ht_create(sizeof(size_t), sizeof(lisp_value));
  
  ht_set(scope->values,&sym.integer,&value);
  return nil;
}

lisp_context * current_context;

lisp_context * lisp_context_new(){
  lisp_context * ctx = GC_MALLOC(sizeof(ctx[0]));
  ctx->next_symbol = 1;
  ctx->symbols = ht_create_strkey(sizeof(size_t));
  ctx->symbols_reverse = ht_create(sizeof(size_t), sizeof(char *));
  ctx->globals = scope_new(NULL);
  var prev_ctx = current_context;
  current_context = ctx;
  rest_sym = get_symbol("&rest");
  if_sym = get_symbol("if");
  quote_sym = get_symbol("quote");
  quasiquote_sym = get_symbol("quasiquote");
  unquote_sym = get_symbol("unquote");
  scope_create_value(current_context->globals, get_symbol("nil"), nil);
  scope_create_value(current_context->globals, get_symbol("t"), t);
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
#define cddr(x) cdr(cdr(x))
#define cdddr(x) cdr(cddr(x))
#define cddddr(x) cdr(cdddr(x))
#define cdddddr(x) cdr(cddddr(x))
#define cddddddr(x) cdr(cdddddr(x))
#define cdddddddr(x) cdr(cddddddr(x))

#define cadr(x) car(cdr(x))
#define caddr(x) car(cddr(x))
#define cadddr(x) car(cdddr(x))
#define caddddr(x) car(cddddr(x))
#define cadddddr(x) car(cdddddr(x))
#define caddddddr(x) car(cddddddr(x))
#define cadddddddr(x) car(cdddddddr(x))

lisp_value new_cons(lisp_value _car, lisp_value _cdr){
  lisp_value v = {.type = LISP_CONS, .cons = GC_MALLOC(sizeof(cons))};
  v.cons->car = _car;
  v.cons->cdr = _cdr;
  return v;
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
	 if(c == '"'){
		c = io_peek_u8(rd);
		if(c == '"'){
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
  lisp_value v = {.type = LISP_STRING, .string = gc_clone(wd.data, strlen(wd.data) + 1) };
  io_writer_clear(&wd);
  return v;
}

int64_t get_symbol_id(const char * s){
  int64_t id = 0;
  if(ht_get(current_context->symbols, &s, &id))
	 return id;
  id = current_context->next_symbol++;
  ht_set(current_context->symbols, &s, &id);
  ht_set(current_context->symbols_reverse, &id, &s);
  return id;
}

lisp_value get_symbol(const char * s){
  if(s == NULL || strlen(s) == 0)
	 error();
  return (lisp_value){.type = LISP_SYMBOL, .integer = get_symbol_id(s)};
}

const char * symbol_name(int64_t id){
  char * out;
  if(ht_get(current_context->symbols_reverse, &id, &out))
	 return out;
  return NULL;
}

void type_assert(lisp_value val, lisp_type type){
  if(val.type != type){
	 printf("Invalid type, expected %s, but got %s\n",
			  lisp_type_to_string(type),
			  lisp_type_to_string(val.type));
	 error();
  }
	 
}

lisp_value parse_token(const char * x, int count){

  char * tp= NULL;
  double o = strtold(x, &tp);
  if(tp == x + count){
	 return (lisp_value) {.type = LISP_RATIONAL, .rational = o }; 
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
	 if(c == ' ' || c == ')' || c == 0 || c == '\n'){
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
  if(c == '\''){
	 io_read_u8(rd);
	 return new_cons(quote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == '`'){
	 io_read_u8(rd);
	 return new_cons(quasiquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == ','){
	 io_read_u8(rd);
	 return new_cons(unquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == '('){
	 io_read_u8(rd);
	 skip_comment_and_whitespace(rd);
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

lisp_value lisp_eval(lisp_scope * scope, lisp_value value);
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
  lisp_value head_value = scope_get_value(scope, head);
  if(head_value.type != LISP_FUNCTION_MACRO) return value;
  lisp_function * f = head_value.function;
  var function_scope = scope_new(f->closure);
  var args = f->args;
  var args2 = cdr(value);
  while(args.type != LISP_NIL){
	 var arg = car(args);
	 var argv = car(args2);
	 
	 if(arg.type != LISP_SYMBOL){
		println(value);
		println(args);
		println(args2);
		printf("arg name must be a symbol\n");
		error();
		return nil;
	 }
	 if(arg.symbol == rest_sym.symbol){
		args = cdr(args);
		arg = car(args);
		if(arg.type != LISP_SYMBOL){
		  printf("(2) arg name must be a symbol\n");
		  error();
		  return nil;
		}
		scope_create_value(function_scope, arg, args2);
		break;
	 }
	 scope_create_value(function_scope, arg, argv);
	 
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


lisp_value lisp_sub_eval(lisp_scope * scope, lisp_value c){
  if(c.type == LISP_NIL) return nil;
  var next = cdr(c);
  if(next.type != LISP_NIL){
	 var value = lisp_eval(scope, car(c));
	 var nextr = lisp_sub_eval(scope, next);
	 return new_cons(value, nextr);
  }
  return new_cons(lisp_eval(scope, car(c)), nil);
}
lisp_value lisp_eval_quasiquoted(lisp_scope * scope, lisp_value value);

lisp_value lisp_eval_quasiquoted_sub(lisp_scope * scope, lisp_value value){
  if(is_nil(value)) return value;
  var current = car(value);
  var next = cdr(value);
  var value2 = lisp_eval_quasiquoted(scope, current);
  if(next.type != LISP_NIL){
	 var nextr = lisp_eval_quasiquoted_sub(scope, next);
	 if(lisp_value_eq(current, value2) && lisp_value_eq(next, nextr))
		return value;
	 return new_cons(value2, nextr);
  }else{
	 if(lisp_value_eq(current, value))
		return value;
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
		return lisp_eval_quasiquoted_sub(scope, value);
	 }
  default:
	 return value;
  }
}

lisp_value lisp_eval(lisp_scope * scope, lisp_value value){
  switch(value.type){
  case LISP_CONS:
	 {
		lisp_value first_value = lisp_eval(scope, car(value));
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
		  case LISP_UNQUOTE:
			 printf("Unexpected unquote!\n");
			 error();
			 break;
		  case LISP_LET:
			 {
				var argform = cadr(value);
				scope = scope_new(scope);
				while(argform.type != LISP_NIL){
				  var arg = car(argform);
				  var sym = car(arg);
				  var value = lisp_eval(scope, cadr(arg));
				  scope_create_value(scope, sym, value);
				  argform = cdr(argform);
				}
				value = cdr(value);
				goto progn_case;
			 }
		  case LISP_PROGN:
		  progn_case:
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
				lisp_function * f = GC_MALLOC(sizeof(*f));
				f->code = body;
				f->args = args;
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
				var value = lisp_eval(scope, caddr(value));
				scope_set_value(scope, sym, value);
				return value;
			 }
		  case LISP_DEFINE:
			 {
				var sym = cadr(value);
				if(sym.type != LISP_SYMBOL){
				  return nil; // error
				}
				var value = lisp_eval(scope, caddr(value));
				while(scope->super != NULL)
				  scope = scope->super;

				scope_create_value(scope, sym, value);
				return value;
			 }
		  case LISP_EVAL:
			 {
				return lisp_eval(scope, lisp_eval(scope, cadr(value)));
			 }
		  }
		}
		lisp_value things = lisp_sub_eval(scope, cdr(value));
		
		if(first_value.type == LISP_FUNCTION_NATIVE){
		  var n = first_value.nfunction;
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
			 return f.n1(car(things));
		  case 2:
			 return f.n2(car(things), cadr(things));
		  case 3:
			 return f.n3(car(things), cadr(things), caddr(things));
		  case 4:
			 return f.n4(car(things), cadr(things), caddr(things), cadddr(things));
		  case 5:
			 return f.n5(car(things), cadr(things), caddr(things), cadddr(things), caddddr(things));
		  case 6:
			 return f.n6(car(things), cadr(things), caddr(things), cadddr(things), caddddr(things), cadddddr(things));
		  default:
			 printf("Unsupported number of args");
		  }
		}else if(first_value.type == LISP_FUNCTION){
		  
		  var f = first_value.function;
		  var function_scope = scope_new(f->closure);
		  var args = f->args;
		  var args2 = things;
		  while(args.type != LISP_NIL){
			 var arg = car(args);
			 if(arg.type != LISP_SYMBOL){
				// error
				printf("arg name must be a symbol");
				return nil;
			 }
			 if(arg.symbol == rest_sym.symbol){
				args = cdr(args);
				arg = car(args);
				if(arg.type != LISP_SYMBOL){
				  // error
				  println(arg);
				  printf("(2) arg name must be a symbol");
				  return nil;
				}
				scope_create_value(function_scope, arg, args2);
				break;
			 }
			 var argv = car(args2);
			 scope_create_value(function_scope, arg, argv);
			 
			 args = cdr(args);
			 args2 = cdr(args2);
		  }

		  var it = f->code;
		  lisp_value ret = nil;
		  while(it.type != LISP_NIL){
			 //printf("-->");println(car(it));
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
			 case 2:
				r = f.n2(car(things).integer, cadr(things).integer);
				break;
			 default:
				
				printf("invalid argnr for "); print(value); printf("\n");
				error();
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
				printf("unsupported number of arguments\n");
				error();
			 }
		  }
		  var ret = func->return_example;
		  ret.integer = r;
		  return ret;
		  		  
		}else{
		  printf("not a function :");
		  println(value);
		  error();
		  return nil;
		}
	 }
	 break;
  case LISP_SYMBOL:
	 if(scope_try_get_value(scope, value, &value))
		return value;
	 else{
		printf("Symbol ");print(value); printf(" not found\n");
		error();
	 }
		 
  default:
	 return value;
  }
  return nil;
}

lisp_value lisp_eval_stream(io_reader * rd){
  lisp_value result = nil;
  while(true){
	 var off = rd->offset;
	 var code = lisp_read_stream(rd);
	 if(off == rd->offset || code.type == LISP_NIL) break;
	 while(true){

		var next_code = lisp_macro_expand(current_context->globals, code);
		if(lisp_value_eq(next_code, code))
		  break;
		code = next_code;
	 }
	 println(code);
	 result = lisp_eval(current_context->globals, code);
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


lisp_value print(lisp_value v){
  lisp_value iv = v;
  switch(v.type){
  case LISP_NIL:
	 printf("()");
	 break;
  case LISP_T:
	 printf("t");
	 break;
  case LISP_INTEGER:
	 printf("%i", v.integer);
	 break;
  case LISP_NATIVE_POINTER:
	 if(v.integer == 0)
		printf("NULL");
	 else
		printf("%p", v.integer);
	 break;
  case LISP_ALIEN_FUNCTION:
	 printf("ALIENF %p", v.alien_func->func);
	 break;
  case LISP_RATIONAL:
	 printf("%f", v.rational);
	 break;
  case LISP_STRING:
	 printf("\"%s\"", v.string);
	 break;
  case LISP_SYMBOL:
	 printf("%s", symbol_name(v.integer));
	 break;
  case LISP_FUNCTION:
	 printf("FUNCTION");
	 break;
  case LISP_FUNCTION_MACRO:
	 printf("FUNCTION_MACRO");
	 break;
  case LISP_FUNCTION_NATIVE:
	 printf("Native function");
	 break;
  case LISP_MACRO_BUILTIN:
	 printf("MacroBuiltin");
	 break;
  case LISP_CONS:
	 printf("(");
	 var first = true;
	 while(v.type == LISP_CONS){
		if(first){
		  first = false;
		}else{
		  printf(" ");
		}
		print(v.cons->car);
		v = v.cons->cdr;
	 }
	 if(v.type != LISP_NIL){
		printf(" . ");
		print(v);
	 }
	 printf(")");
	 break;
  }
  return iv;

}
void lisp_register_value(const char * name, lisp_value value){
  scope_create_value(current_context->globals, get_symbol(name), value);
}
void lisp_register_native(const char * name, int nargs, void * fptr){
  native_function *nf= GC_MALLOC(sizeof(*nf));
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
  if(a.type == LISP_RATIONAL)
	 a.rational += b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer += b.integer;
  return a;
}

lisp_value lisp_sub(lisp_value a, lisp_value b){
  if(a.type == LISP_RATIONAL)
	 a.rational -= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer -= b.integer;
  return a;
}

lisp_value lisp_mul(lisp_value a, lisp_value b){
  if(a.type == LISP_RATIONAL)
	 a.rational *= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer *= b.integer;
  return a;
}
lisp_value lisp_div(lisp_value a, lisp_value b){
  if(a.type == LISP_RATIONAL)
	 a.rational /= b.rational;
  else if(a.type == LISP_INTEGER)
	 a.integer /= b.integer;
  return a;
}

lisp_value lisp_less(lisp_value a, lisp_value b){
  if(a.type == LISP_RATIONAL && a.rational < b.rational)
	 return t;
  if(a.type == LISP_INTEGER && a.integer < b.integer)
	 return t;
  return nil;
}
lisp_value lisp_greater(lisp_value a, lisp_value b){
  if(a.type == LISP_RATIONAL && a.rational > b.rational)
	 return t;
  if(a.type == LISP_INTEGER && a.integer > b.integer)
	 return t;
  return nil;
}

lisp_value lisp_eq(lisp_value a, lisp_value b){
  return _lisp_eq(a,b) ? t : nil;
}

lisp_value lisp_len(lisp_value a){
  int64_t i = 0;
  while(a.type == LISP_CONS){
	 i += 1;
	 a = a.cons->cdr;
  }
  return (lisp_value){.type = LISP_INTEGER, .integer = i};
}

lisp_value lisp_error(lisp_value a){
  println(a);
  error();
  return nil;
}

lisp_value println(lisp_value v){
  print(v);
  printf("\n");
  return v;
}

lisp_value lisp_integer(lisp_value v){
  if(v.type == LISP_RATIONAL){
	 v.integer = (int64_t) v.rational;
  }
  v.type = LISP_INTEGER;
  return v;
}

lisp_value lisp_panic(lisp_value v){
  printf("Panic: ");print(v);printf("\n");
  error();
  return nil;
}

lisp_value integer(int64_t v){
  return (lisp_value){.type = LISP_INTEGER, .integer = v};
}

lisp_value gc_heap(){
  return integer(GC_get_heap_size());
}

lisp_value load_lib(lisp_value str){
  if(str.type != LISP_STRING) {
	 lisp_error((lisp_value){.type = LISP_STRING, .string = (void *)"not a string"});
	 return nil;
  }
  printf("Loading: %s\n", str.string);
  void * handle = dlopen(str.string, RTLD_GLOBAL |RTLD_NOW);
  char * err = dlerror();
  if(err != NULL) printf("DL error: %s\n", err);
  if(handle == NULL) return nil;
  return (lisp_value){.type = LISP_NATIVE_POINTER, .integer = (int64_t) handle};
}

lisp_value lisp_alien_func(lisp_value lib, lisp_value str, lisp_value return_example, lisp_value arg_example){
  
  void * handle = (void *) lib.integer;
  void * sym = dlsym(handle, str.string);
  if(sym == NULL) {
	 printf("No such function %s\n", str.string);
	 return nil;
  }
  alien_function * f = GC_MALLOC(sizeof(*f));
  f->func = sym;
  f->return_example = return_example;
  f->arg_example = arg_example;
  if(sym != NULL)
	 return (lisp_value){.type = LISP_ALIEN_FUNCTION, .alien_func = f};
  return nil;
}

lisp_value lisp_wrap_func(lisp_value lib, lisp_value str, lisp_value argcnt){
  
  void * handle = (void *) lib.integer;
  void * sym = dlsym(handle, str.string);
  if(sym == NULL) {
	 printf("No such function %s\n", str.string);
	 return nil;
  }
  native_function * f = GC_MALLOC(sizeof(*f));
  f->fptr = sym;
  f->nargs = argcnt.integer;
  if(sym != NULL)
	 return (lisp_value){.type = LISP_FUNCTION_NATIVE, .nfunction = f};
  return nil;
}
lisp_value lisp_rational(lisp_value v){
  if(v.type != LISP_RATIONAL){
	 v.rational = (double)v.integer;
  }
  v.type = LISP_RATIONAL;
  return v;
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
  }
  error();
  return NULL;
}

lisp_value lisp_type_of(lisp_value v){
  return get_symbol(lisp_type_to_string(v.type));
}

lisp_value lisp_load(lisp_value v){
  type_assert(v, LISP_STRING);
  return lisp_eval_file(v.string);
}

 int main(int argc, char ** argv){
  ht_mem_malloc = GC_malloc;
  ht_mem_free = GC_free;
  current_context = lisp_context_new();
  lisp_register_native("gc-heap", 0, gc_heap);
  lisp_register_native("+", 2, lisp_add);
  lisp_register_native("-", 2, lisp_sub);
  lisp_register_native("*", 2, lisp_mul);
  lisp_register_native("/", 2, lisp_div);
  lisp_register_native("<", 2, lisp_less);
  lisp_register_native(">", 2, lisp_greater);
  lisp_register_native("print", 1, print);
  lisp_register_native("println", 1, println);
  lisp_register_native("car", 1, car);
  lisp_register_native("cdr", 1, cdr);
  lisp_register_native("cons", 2, new_cons);
  lisp_register_native("=", 2, lisp_eq);
  lisp_register_native("panic", 1, lisp_error);
  lisp_register_native("integer", 1, lisp_integer);
  lisp_register_native("rational", 1, lisp_rational);
  lisp_register_native("sin", 1, lisp_sin);
  lisp_register_native("type-of", 1, lisp_type_of);
  
  lisp_register_native("load-lib", 1, load_lib);
  lisp_register_native("load-alien", 4, lisp_alien_func);
  lisp_register_native("load-wrap", 3, lisp_wrap_func);
  lisp_register_native("read-string", 1, lisp_read);
  lisp_register_native("panic", 1, lisp_panic);
  lisp_register_native("load", 1, lisp_load);

  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  lisp_register_macro("let", LISP_LET);
  lisp_register_macro("progn", LISP_PROGN);
  lisp_register_macro("lambda", LISP_LAMBDA);
  lisp_register_macro("loop", LISP_LOOP);
  lisp_register_macro("macro", LISP_MACRO);
  lisp_register_macro("set", LISP_SET);
  lisp_register_macro("define", LISP_DEFINE);
  lisp_register_macro("eval", LISP_EVAL);
  lisp_register_macro("quasiquote", LISP_QUASIQUOTE);
  lisp_register_macro("unquote", LISP_UNQUOTE);

  lisp_register_value("native-null-pointer", (lisp_value){.type = LISP_NATIVE_POINTER, .integer = 0});

  for(int i = 1; i < argc; i++)
	 lisp_eval_file(argv[i]);  
  return 0;
}