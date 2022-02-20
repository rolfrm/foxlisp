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
		LISP_MACRO_BUILTIN = 10,
		LISP_NATIVE_POINTER = 11,
		LISP_ALIEN_FUNCTION = 12
		
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
typedef struct _cons cons;
typedef struct __lisp_function lisp_function;
typedef struct __native_function native_function;
typedef struct __alien_function alien_function;
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
	 alien_function * alien_func;
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
lisp_value quote_sym = {.type = LISP_SYMBOL};

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

struct __alien_function {
  void * func;
  lisp_value return_example;
  lisp_value arg_example;
};

lisp_value print(lisp_value v);
lisp_value get_symbol(const char * s);
lisp_value println(lisp_value v);
lisp_value lisp_len(lisp_value lst);
