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
				  LISP_UNQUOTE_SPLICE = 13,
              LISP_SYMBOL_VALUE = 14,
              LISP_BOUND = 15,
              LISP_WITH_EXCEPTION_HANDLER = 16
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
  cons * lookup;
  size_t argcnt;
  bool lookup_on_stack;
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
extern lisp_value nil;
extern lisp_value t;

extern lisp_value rest_sym;
extern lisp_value if_sym;
extern lisp_value quote_sym;
extern lisp_value quasiquote_sym;
extern lisp_value unquote_sym;
extern lisp_value unquote_splice_sym;

void foxlist_thread_init();
void gc_collect_garbage(lisp_context * context);
void * gc_clone(const void * mem, size_t s);
// functions
lisp_value lisp_eval(lisp_scope * scope, lisp_value value);

lisp_value print(lisp_value v);
lisp_value get_symbol(const char * s);
lisp_value println(lisp_value v);
lisp_value lisp_len(lisp_value lst);
const char * lisp_type_to_string(lisp_type t);

lisp_value new_cons(lisp_value a, lisp_value b);
lisp_value copy_cons(lisp_value a);
lisp_value lisp_length(lisp_value lst);

lisp_value car(lisp_value v);
lisp_value cdr(lisp_value v);
lisp_value cadr(lisp_value v);
lisp_value cddr(lisp_value v);
lisp_value pop(lisp_value * v);
lisp_value lisp_append(lisp_value a, lisp_value b);
void * lisp_malloc(size_t s);
//void * lisp_realloc(void * p, size_t v);

lisp_value get_symbol(const char * s);
bool eq(lisp_value a, lisp_value b);
const char * symbol_name(int64_t id);

lisp_context * lisp_context_new();
lisp_value make_vector(lisp_value len, lisp_value _default);
lisp_value vector_length(lisp_value v);
lisp_value vector_ref(lisp_value _vector, lisp_value k);
lisp_value vector_set(lisp_value vector, lisp_value k, lisp_value v);
lisp_value vector_elem_type(lisp_value vector);
lisp_value vector_copy(lisp_value vector);
lisp_value integer(int64_t v);
lisp_value rational(double v);
lisp_value float32(float v);

lisp_value byte(unsigned char v);
lisp_value native_pointer(void * ptr);
lisp_value lisp_rational(lisp_value value);
lisp_value lisp_float32(lisp_value value);


size_t lisp_type_size(lisp_type type);
bool eq(lisp_value a, lisp_value b);
bool is_nil(lisp_value a);
lisp_value lisp_error(lisp_value v);
void raise_string(const char * str);
bool type_assert(lisp_value val, lisp_type type);
bool elem_type_assert(lisp_value vector, lisp_type type);

lisp_scope * lisp_scope_new(lisp_scope * super);
lisp_scope * lisp_scope_new2(lisp_scope * super, cons * argsbuffer, size_t cnt);
bool lisp_scope_try_get_value(lisp_scope * scope, lisp_value sym, lisp_value * out);
lisp_value lisp_scope_set_value(lisp_scope * scope, lisp_value sym, lisp_value value);

lisp_scope * lisp_context_get_root_scope();

void lisp_register_value(const char * name, lisp_value value);
void lisp_register_native(const char * name, int nargs, void * fptr);
// macros

#define cdddr(x) cdr(cddr(x))
#define cddddr(x) cdr(cdddr(x))
#define cdddddr(x) cdr(cddddr(x))
#define cddddddr(x) cdr(cdddddr(x))
#define cdddddddr(x) cdr(cddddddr(x))

#define caddr(x) car(cddr(x))
#define cadddr(x) car(cdddr(x))
#define caddddr(x) car(cddddr(x))
#define cadddddr(x) car(cdddddr(x))
#define caddddddr(x) car(cddddddr(x))
#define cadddddddr(x) car(cdddddddr(x))

#define TYPE_ASSERT(v, t) if(!type_assert(v, t)) return nil;
