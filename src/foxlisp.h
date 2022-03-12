// enums
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
		LISP_ALIEN_FUNCTION = 12,
		LISP_VECTOR = 13,
		LISP_BYTE = 14,
		LISP_FLOAT32
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
				  LISP_DEFINE = 9,
				  LISP_EVAL = 10,
				  LISP_QUASIQUOTE = 11,
				  LISP_UNQUOTE = 12,
				  LISP_UNQUOTE_SPLICE = 13
}lisp_builtin;

// structs
typedef int64_t lisp_symbol;
typedef struct _cons cons;
typedef struct __lisp_function lisp_function;
typedef struct __native_function native_function;
typedef struct __alien_function alien_function;
typedef struct __lisp_vector lisp_vector;
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
	 void * native_pointer;
	 lisp_vector * vector;
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

typedef struct __lisp_scope lisp_scope;

struct __lisp_scope{
  lisp_scope * super;
  hash_table * values;
};

typedef struct{
  hash_table * symbols;
  hash_table * symbols_reverse;
  size_t next_symbol;
  lisp_scope * globals;
}lisp_context;

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

struct __lisp_vector{
  void * data;
  size_t count;
  size_t elem_size;
  lisp_value default_value;
};

// globals
const lisp_value nil = {0};
const lisp_value t = {.type = LISP_T};

lisp_value rest_sym = {.type = LISP_SYMBOL};
lisp_value if_sym = {.type = LISP_SYMBOL};
lisp_value quote_sym = {.type = LISP_SYMBOL};
lisp_value quasiquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_sym = {.type = LISP_SYMBOL};
lisp_value unquote_splice_sym = {.type = LISP_SYMBOL};

// functions
lisp_value print(lisp_value v);
lisp_value get_symbol(const char * s);
lisp_value println(lisp_value v);
lisp_value lisp_len(lisp_value lst);
const char * lisp_type_to_string(lisp_type t);

lisp_value new_cons(lisp_value a, lisp_value b);

lisp_value car(lisp_value v);
lisp_value cdr(lisp_value v);
lisp_value pop(lisp_value * v);
lisp_value lisp_append(lisp_value a, lisp_value b);
void * lisp_malloc(size_t s);
void * lisp_realloc(void * p, size_t v);

lisp_value get_symbol(const char * s);
bool eq(lisp_value a, lisp_value b);
const char * symbol_name(int64_t id);

lisp_context * lisp_context_new();
lisp_value vector_length(lisp_value v);
lisp_value vector_ref(lisp_value _vector, lisp_value k);
lisp_value vector_set(lisp_value vector, lisp_value k, lisp_value v);
lisp_value vector_elem_type(lisp_value vector);
lisp_value vector_copy(lisp_value vector);
lisp_value integer(int64_t v);
bool eq(lisp_value a, lisp_value b);
bool is_nil(lisp_value a);
lisp_value lisp_error(lisp_value v);
void error_print(const char * str);
void type_assert(lisp_value val, lisp_type type);

lisp_scope * lisp_scope_new(lisp_scope * super);
bool lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out);
lisp_value lisp_scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value);

lisp_scope * lisp_context_get_root_scope();

// macros
#define POP(x)pop(&x)

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

