#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <microio.h>
#include <stdio.h>
#include <iron/full.h>
typedef struct _cons cons;

typedef enum {
		LISP_NIL,
		LISP_T,
		LISP_CONS,
		LISP_INTEGER,
		LISP_RATIONAL,
		LISP_STRING,
		LISP_SYMBOL,
		LISP_FUNCTION,
		LISP_FUNCTION_NATIVE,
		LISP_MACRO_BUILTIN
}lisp_type;

typedef enum {
				  LISP_IF = 1,
				  LISP_QUOTE = 2
}lisp_builtin;

typedef struct __lisp_function lisp_function;
typedef struct __native_function native_function;
typedef struct{
  lisp_type type;
  union{
	 cons * cons;
	 int64_t integer;
	 double rational;
	 char * string;
	 lisp_function * function;
	 native_function * nfunction;
	 lisp_builtin builtin;
  };
}lisp_value;


struct __lisp_function{
  lisp_value code;
  int nargs;
};

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

lisp_value print(lisp_value v);


lisp_scope * scope_new(lisp_scope * super){
  lisp_scope * s = malloc(sizeof(*super));
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

lisp_value scope_create(lisp_scope * scope, lisp_value sym, lisp_value value){
  if(scope->values == NULL)
	 scope->values = ht_create(sizeof(size_t), sizeof(lisp_value));
  
  ht_set(scope->values,&sym.integer,&value);
  return nil;
}

lisp_context * current_context;

lisp_context * lisp_context_new(){
  lisp_context * ctx = malloc(sizeof(ctx[0]));
  ctx->next_symbol = 1;
  ctx->symbols = ht_create_strkey(sizeof(size_t));
  ctx->symbols_reverse = ht_create(sizeof(size_t), sizeof(char *));
  ctx->globals = scope_new(NULL);
  
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

lisp_value _free_cons = nil;
lisp_value new_cons(lisp_value _car, lisp_value _cdr){
  
  if(_free_cons.type != LISP_NIL){
	 lisp_value v = _free_cons;
	 _free_cons = cdr(_free_cons);
	 v.cons->car = _car;
	 v.cons->cdr = _cdr;
	 return v;
  }
  
  lisp_value v = {.type = LISP_CONS, .cons = calloc(sizeof(cons), 1)};
  v.cons->car = _car;
  v.cons->cdr = _cdr;
  return v;
  return nil;
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
	 if(c == ' ' || c == '\n'){
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
  lisp_value v = {.type = LISP_STRING, .string = wd.data };
  return v;
}

int64_t get_symbol_id(const char * s){
  int64_t id = 0;
  if(ht_get(current_context->symbols, &s, &id))
	 return id;
  id = current_context->next_symbol++;
  ht_set(current_context->symbols, &s, &id);
  ht_set(current_context->symbols_reverse, &id, &s);
  //printf("get symbol: %s %i %i\n", s, id, count);
  return id;
}

lisp_value get_symbol(const char * s){
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
	 if(c == ' ' || c == ')' || c == 0){
		break;
	 }
	 io_read_u8(rd);
	 io_write_u8(&wd, c);
  }
  io_write_u8(&wd, 0);
  lisp_value vv = parse_token(wd.data, wd.offset - 1);
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
		  if(first_value.builtin == LISP_IF){
			 var cond = lisp_eval(scope, cadr(value));
			 if(cond.type == LISP_NIL){
				return lisp_eval(scope, cadddr(value));
			 }else{
				return lisp_eval(scope, caddr(value));
			 }
		  }else if(first_value.builtin == LISP_QUOTE){
			 return cadr(value);
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
  var code = lisp_read_stream(rd);
  return lisp_eval(current_context->globals, code);
}

lisp_value lisp_eval_string(const char * str){
  io_reader w =io_reader_from_bytes((void *)str, strlen(str) + 1);
  w.offset = 0;
  return lisp_eval_stream(&w);
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
	 printf("%s", symbol_name(v.integer));
	 break;
  case LISP_FUNCTION:
	 printf("FUNCTION");
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
  native_function *nf= malloc(sizeof(*nf));
  nf->nargs = nargs;
  nf->fptr = fptr;
  lisp_value v = {
						.type = LISP_FUNCTION_NATIVE,
						.nfunction = nf
  };
  scope_create(current_context->globals, get_symbol(name), v);
}

void lisp_register_macro(const char * name, lisp_builtin builtin){
  lisp_value v = {
						.type = LISP_MACRO_BUILTIN,
						.builtin = builtin
  };
  scope_create(current_context->globals, get_symbol(name), v);
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


int main(int argc, char ** argv){

  current_context = lisp_context_new();
  lisp_register_native("+", 2, lisp_add);
  lisp_register_native("-", 2, lisp_sub);
  lisp_register_native("print", 1, print);
  lisp_register_native("car", 1, car);
  lisp_register_native("cdr", 1, cdr);
  lisp_register_native("cons", 1, new_cons);
  lisp_register_macro("if", LISP_IF);
  lisp_register_macro("quote", LISP_QUOTE);
  
  const char * data = "(+ 123 321)";
  io_reader w =io_reader_from_bytes((void *)data, strlen(data) + 1);
  w.offset = 0;
  
  var v = lisp_read_stream(&w);
  printf(">>> ");
  print(v);
  printf("\n");
  var s = lisp_eval_string("A");
  var num = lisp_eval_string("5");
  scope_create(current_context->globals, s, num);
  var ret = scope_get_value(current_context->globals, s);
  print(ret);printf("\n");

  var ret2 = lisp_eval_string("(print (- (+ 5 3 ) 1))");
  print(ret2);printf("<-- eval\n");
  lisp_eval_string("(print (if 1 (+ 2 3.5) 3))");printf("<-- eval\n");
  lisp_eval_string("(print (quote (1 2 3)))");printf("<-- eval\n");
  
  return 0;
}
