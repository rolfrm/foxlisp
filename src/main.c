#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <signal.h>
#include <stdio.h>
#include <iron/full.h>
#include <gc.h>
void ht_free(hash_table *ht);
extern void * (* ht_mem_malloc)(size_t s);
extern void (* ht_mem_free)(void *);
typedef struct _cons cons;

void error(){
    raise(SIGINT);
}

typedef enum {
		LISP_NIL = 0,
		LISP_T = 1,
		LISP_CONS = 2,
		LISP_INTEGER = 3,
		LISP_RATIONAL= 4 ,
		LISP_STRING = 5,
		LISP_SYMBOL = 6,
		LISP_FUNCTION = 7,
		LISP_FUNCTION_MACRO = 8,
		LISP_FUNCTION_NATIVE = 9,
		LISP_MACRO_BUILTIN = 10
}lisp_type;

typedef enum {
				  LISP_IF = 1,
				  LISP_QUOTE = 2,
				  LISP_LET = 3,
				  LISP_PROGN = 4,
				  LISP_LAMBDA = 5,
				  LISP_LOOP = 6,
				  LISP_MACRO = 7,
				  LISP_SET = 8,
				  LISP_DEFINE = 9
}lisp_builtin;

typedef int64_t lisp_symbol;
typedef struct __lisp_function lisp_function;
typedef struct __native_function native_function;
typedef struct{
  lisp_type type;
  union{
	 cons * cons;
	 int64_t integer;
	 lisp_symbol symbol;
	 double rational;
	 char * string;
	 lisp_function * function;
	 native_function * nfunction;
	 lisp_builtin builtin;
  };
}lisp_value;

struct __native_function{
  void * fptr;
  int nargs;
};

struct _cons {
  lisp_value car;
  lisp_value cdr;
};
const char * symbol_name(int64_t id);
const lisp_value nil = {0};
const lisp_value t = {.type = LISP_T};
lisp_value rest_sym = {.type = LISP_SYMBOL};
lisp_value if_sym = {.type = LISP_SYMBOL};

void * gc_clone(void * mem, size_t s){
  void * d = GC_MALLOC(s);
  memcpy(d, mem, s);
  return d;
}

typedef struct __lisp_scope lisp_scope;
typedef struct{
  hash_table * symbols;
  hash_table * symbols_reverse;
  size_t next_symbol;
  lisp_scope * globals;
}lisp_context;

struct __lisp_scope{
  lisp_scope * super;
  hash_table * values;
};

struct __lisp_function{
  lisp_scope * closure;
  lisp_value code;
  lisp_value args;
  int nargs;
};

lisp_value print(lisp_value v);
lisp_value get_symbol(const char * s);
lisp_value println(lisp_value v);
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

lisp_value scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope == NULL){
	 // error variable not found
	 printf("Error: variable '%s' not found\n", symbol_name(sym.integer));
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

void free_cons(lisp_value cons){
  if(cons.type != LISP_CONS)
	 return; // error
  lisp_value cons_end = cons;
  while(cons_end.cons->cdr.type != LISP_NIL){
	 cons_end = cons_end.cons->cdr;
  }
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

  io_reader w =io_reader_from_bytes((void *)str, strlen(str) + 1);
  w.offset = 0;
  return lisp_read_stream(&w);
}

lisp_value lisp_eval(lisp_scope * scope, lisp_value value);
lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value);

lisp_value lisp_sub_macro_expand(lisp_scope * scope, lisp_value c){
  if(c.type == LISP_NIL) return nil;
  var next = cdr(c);
  if(next.type != LISP_NIL){
	 var value = lisp_macro_expand(scope, car(c));
	 var nextr = lisp_sub_macro_expand(scope, next);
	 return new_cons(value, nextr);
  }
  return new_cons(lisp_macro_expand(scope, car(c)), nil);
}

lisp_value lisp_macro_expand(lisp_scope * scope, lisp_value value){
  if(value.type != LISP_CONS){
	 return value;
  }
  value = lisp_sub_macro_expand(scope, value);
  lisp_value head = car(value);
  if(head.type != LISP_SYMBOL) return value;
  lisp_value head_value = scope_get_value(scope, head);
  if(head_value.type != LISP_FUNCTION_MACRO) return value;
  lisp_function * f = head_value.function;
  var function_scope = scope_new(f->closure);
  var args = f->args;
  var args2 = cdr(value);
  while(args.type != LISP_NIL && args2.type != LISP_NIL){
	 var arg = car(args);
	 var argv = car(args2);
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
		  printf("(2) arg name must be a symbol");
		  return nil;
		}
		scope_create_value(function_scope, arg, args2);
		break;
	 }
	 scope_create_value(function_scope, arg, argv);
	 
	 args = cdr(args);
	 args2 = cdr(args2);
	 
  }
  return lisp_eval(function_scope, f->code);
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
		  case LISP_LAMBDA:
			 {
				var args = cadr(value);
				var body = caddr(value);
				lisp_function * f = GC_MALLOC(sizeof(*f));
				f->code = body;
				f->args = args;
				f->closure = scope;
				return (lisp_value){.type = LISP_FUNCTION, .function = f};
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
		  case LISP_MACRO:
			 {
				var args = cadr(value);
				var body = caddr(value);
				lisp_function * f = GC_MALLOC(sizeof(*f));
				f->code = body;
				f->args = args;
				f->closure = scope;
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
		  while(args.type != LISP_NIL && args2.type != LISP_NIL){
			 var arg = car(args);
			 var argv = car(args2);
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
				  printf("(2) arg name must be a symbol");
				  return nil;
				}
				scope_create_value(function_scope, arg, args2);
				break;
			 }
			 scope_create_value(function_scope, arg, argv);
			 
			 args = cdr(args);
			 args2 = cdr(args2);
			 
		  }
		  return lisp_eval(function_scope, f->code);
		 
		}else{
		  printf("not a function :");
		  print(value);
		  return nil;
		}
	 }
	 break;
  case LISP_SYMBOL:
	 return scope_get_value(scope, value);
	 break;
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
	 code = lisp_macro_expand(current_context->globals, code);
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
  case LISP_RATIONAL:
	 printf("%f", v.rational);
	 break;
  case LISP_STRING:
	 printf("%s", v.string);
	 break;
  case LISP_SYMBOL:
	 printf("'%s' %i", symbol_name(v.integer), v.integer);
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

void lisp_register_native(const char * name, int nargs, void * fptr){
  native_function *nf= GC_MALLOC(sizeof(*nf));
  nf->nargs = nargs;
  nf->fptr = fptr;
  lisp_value v = {
						.type = LISP_FUNCTION_NATIVE,
						.nfunction = nf
  };
  scope_create_value(current_context->globals, get_symbol(name), v);
}

void lisp_register_macro(const char * name, lisp_builtin builtin){
  lisp_value v = {
						.type = LISP_MACRO_BUILTIN,
						.builtin = builtin
  };
  scope_create_value(current_context->globals, get_symbol(name), v);
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
  if(a.type == b.type && a.integer == b.integer)
	 return t;
  return nil;
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
lisp_value integer(int64_t v){
  return (lisp_value){.type = LISP_INTEGER, .integer = v};
}
lisp_value gc_heap(){
  return integer(GC_get_heap_size());
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
  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  lisp_register_macro("let", LISP_LET);
  lisp_register_macro("progn", LISP_PROGN);
  lisp_register_macro("lambda", LISP_LAMBDA);
  lisp_register_macro("loop", LISP_LOOP);
  lisp_register_macro("macro", LISP_MACRO);
  lisp_register_macro("set", LISP_SET);
  lisp_register_macro("define", LISP_DEFINE);
  
  const char * data = "(+ 123 321)";
  io_reader w = io_reader_from_bytes((void *)data, strlen(data) + 1);
  w.offset = 0;
  
  var v = lisp_read_stream(&w);
  printf(">>> ");
  print(v);
  printf("\n");
  var s = lisp_eval_string("A");
  var num = lisp_eval_string("5");
  scope_create_value(current_context->globals, s, num);
  var ret = scope_get_value(current_context->globals, s);
  print(ret);printf("\n");

  var ret2 = lisp_eval_string("(print (- (+ 5 3 ) 1))");
  print(ret2);printf("<-- eval\n");
  lisp_eval_string("(println (if 1 (+ 2 3.5) 3))");
  lisp_eval_string("(println (quote (1 2 3)))");
  lisp_eval_string("(println (cons 1 2))");
  lisp_eval_string("(println (+ 1 2))");
  lisp_eval_string("(println (lambda (x) x))");
  lisp_eval_string("(define y (+ 0.33333 5))");
  lisp_eval_string("(println (+ y y))");
  lisp_eval_string("((lambda (x y) (print (+ x y))) 5 50)");
  lisp_eval_string("(define add (lambda (x y) (+ x y)))");
  lisp_eval_string("(println (add 123 321))");
  lisp_eval_file("fox1.lisp");
  
  return 0;
}
