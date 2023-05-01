// enums
typedef enum {
  LISP_NIL = 0,
  LISP_T = 1,
  LISP_CONS = 2,
  LISP_INTEGER = 3,
  LISP_RATIONAL = 4,
  LISP_STRING = 5,
  LISP_SYMBOL = 6,
  LISP_FUNCTION = 7,
  LISP_FUNCTION_MACRO = 8,
  LISP_FUNCTION_NATIVE = 9,
  LISP_MACRO_BUILTIN = 10,
  LISP_NATIVE_POINTER = 11,
  // An indexable array of values from a pool allocator.
  LISP_VECTOR,
  // An indexable array of values, allocated on the heap.
  LISP_NATIVE_VECTOR,
  LISP_BYTE,
  LISP_FLOAT32,
  LISP_HASHTABLE,
  LISP_INTEGER32,
  LISP_SCOPE,
  LISP_GLOBAL_INDEX,
  LISP_LOCAL_INDEX,
  // marks a GC-managed pointer
  LISP_ALLOCATED_POINTER,
  // a native pointer to a lisp_value

  LISP_NATIVE_POINTER_TO_VALUE,
  // internal
  LISP_GLOBAL_CONS_ARRAYS,
  LISP_VALUE_SET,
  LISP_ARRAY,
  LISP_TYPESPEC
} lisp_type;

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
  LISP_QUASIQUOTE = 11,
  LISP_UNQUOTE = 12,
  LISP_UNQUOTE_SPLICE = 13,
  LISP_SYMBOL_VALUE = 14,
  LISP_BOUND,
  LISP_WITH_EXCEPTION_HANDLER,
  LISP_CASE,
  LISP_COND,
  LISP_AND,
  LISP_OR,
  LISP_GET_SCOPE,
  LISP_GET_SCOPE_UNSAFE,
  LISP_REF,

  // added for optimization
  LISP_CAR,
  LISP_CDR,
  LISP_EQN,
  LISP_CDDR,
  LISP_CADR,
  LISP_IS_SYMBOL,
  LISP_IS_LIST,
  LISP_PLOOKUP,
  LISP_NEW_CONS,
  LISP_ADD,
  LISP_SUB,
  LISP_MUL,
  LISP_DIV,
  LISP_CONV_FLOAT32,
  LISP_VECTOR_LENGTH,
  LISP_VECTOR_REF,
  LISP_VECTOR_SET

} lisp_builtin;

// structs
typedef int64_t lisp_symbol;
typedef struct _cons cons;
typedef struct __lisp_function lisp_function;
typedef struct __native_function native_function;
typedef native_function lisp_native_function;

typedef struct __lisp_vector lisp_vector;
typedef struct __lisp_scope lisp_scope;
typedef struct __lisp_typespec lisp_typespec;

typedef struct __attribute__((__packed__)) {
  int8_t scope_type;
  int16_t scope_level;
  int scope_index : 24;
} lisp_local_index;

typedef enum {
  LISP_HASHTABLE_WEAK_KEYS = 1,
  LISP_HASHTABLE_WEAK_VALUES = 2

} lisp_hashtable_flags;

typedef struct {
  union {
	 lisp_type type;
    u64 reserved;
  };
  union {
    cons *cons;
    int64_t integer;
    lisp_symbol symbol;
    double rational;
    char *string;
    lisp_function *function;
    native_function *nfunction;
    lisp_builtin builtin;
    void *native_pointer;
    lisp_vector *vector;
    lisp_scope *scope;
    lisp_local_index local_index;
    void *pointer;
    lisp_typespec *typespec;
  };
} lisp_value;
void lisp_push_scope(lisp_scope *scope);
void lisp_pop_scope(lisp_scope *scope);
struct __native_function {
  void *fptr;
  int nargs; // -1: infinite number of arguments.
  bool eval_args;
  bool macro_like;
  
};

struct _cons {
  lisp_value car;
  lisp_value cdr;
};

struct __lisp_scope {
  lisp_scope *super;
  hash_table *values_index;
  lisp_value *values;
  lisp_symbol *value_symbol;
  size_t values_count;
  size_t values_capacity;
  cons *lookup;
  size_t argcnt;
  bool stack_scope;
  bool lookup_on_stack;
  lisp_scope *sub_scope;
  int non_heap_mark;
};

typedef struct __gc_context gc_context;

typedef struct {
  hash_table *symbols;
  hash_table *symbols_reverse;
  size_t next_symbol;
  lisp_scope *globals;
  gc_context *gc;
} lisp_context;

struct __lisp_function {
  lisp_scope *closure;
  lisp_value code;
  lisp_value args;
  int nargs;
  bool eval_args;
};

struct __lisp_vector {
  void *data;
  size_t count;
  size_t elem_size;
  lisp_value default_value;
};

typedef struct {
  lisp_value *array;
  size_t count;
} lisp_array;

// internal structs
typedef struct {
  cons *array;
  size_t count;
} t_cons_array;

typedef struct {

  t_cons_array *arrays;
  size_t count;
  size_t capacity;
} t_cons_arrays;

struct __lisp_typespec {
  lisp_value name;
  // print function
  lisp_value print;
  // construct function
  lisp_value construct;
  // destruct function
  lisp_value destruct;
  // generic function lookup.
  lisp_value generic_lookup;
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
extern lisp_value else_sym;
extern size_t conses_allocated;
// extern lisp_value lisp_stack;
//  lisp value
bool is_nil(lisp_value v);
bool is_t(lisp_value v);
bool is_cons(lisp_value v);
bool is_integer(lisp_value v);
bool is_float(lisp_value a);
bool is_float32(lisp_value a);
bool is_string(lisp_value a);
bool is_symbol(lisp_value a);
bool is_scope(lisp_value v);
bool is_regular_symbol(lisp_value a);
bool is_function(lisp_value a);
bool is_function_macro(lisp_value a);
bool is_function_native(lisp_value a);
bool is_macro_builtin(lisp_value a);
bool is_float_type(lisp_type t);
bool is_number_type(lisp_type t);
f64 lisp_value_rational(lisp_value v);
lisp_value rational_lisp_value(double o);

int64_t lisp_value_as_integer(lisp_value v);
f64 lisp_value_as_rational(lisp_value v);
lisp_value string_lisp_value(const char *str);
char *lisp_value_string(lisp_value v);
lisp_value integer_lisp_value(i64 i);
int64_t lisp_value_integer(lisp_value v);
int64_t lisp_value_integer_checked(lisp_value v);
lisp_vector *lisp_value_vector_checked(lisp_value v);
lisp_value byte_lisp_value(u8 i);
u8 lisp_value_byte(lisp_value v);
void *lisp_value_pointer(lisp_value val);
lisp_value allocated_pointer_lisp_value(void *ptr);
lisp_value lisp_pointer_to_lisp_value(lisp_value *ptr);
lisp_vector *lisp_value_vector(lisp_value val);
lisp_value vector_lisp_value(lisp_vector *vector);
lisp_value native_vector_lisp_value(lisp_vector *vector);
lisp_type lisp_value_type(lisp_value val);
lisp_value symbol_lisp_value(lisp_symbol sym);
lisp_symbol lisp_value_symbol(lisp_value val);
lisp_value cons_lisp_value(cons *cns);
cons *lisp_value_cons(lisp_value val);
lisp_value function_lisp_value(lisp_function *f);
lisp_function *lisp_value_function(lisp_value val);
lisp_native_function *lisp_value_native_function(lisp_value val);
lisp_value float32_lisp_value(f32 v);
lisp_value native_pointer_lisp_value(void *v);
hash_table *lisp_value_hashtable(lisp_value v);
lisp_value hashtable_lisp_value(hash_table *ht);
bool lisp_value_eq(lisp_value a, lisp_value b);
lisp_value bool_lisp_value(bool v);
bool is_keyword(lisp_value sym);
lisp_value global_index_lisp_value(size_t i);
lisp_value local_index_lisp_value(size_t scope_level, size_t scope_index,
                                  bool scope_type);

typedef struct __lisp_set lisp_set;

lisp_value lisp_set_lisp_value(lisp_set *set);
lisp_value lisp_set_push(lisp_value set, lisp_value obj);
lisp_value lisp_set_pop(lisp_value set, lisp_value index);
lisp_value lisp_set_new();

lisp_value array_lisp_value(lisp_array *array);

void *lisp_pin(lisp_value value);
void *lisp_pin_array(lisp_array *array);
lisp_value lisp_unpin(void *p);

void lisp_pin_args(cons *argslist, size_t cnt);
void lisp_unpin_args(cons *argslist, size_t cnt);

bool equals(lisp_value v1, lisp_value v2);
//
void foxlist_thread_init();
// mark an sweep garbage collector
void gc_collect_garbage(lisp_context *context);
void maybe_gc(lisp_context * ctx);

// the three functions bellow are called by gc_collect_garbage
// clear the GC marks
void gc_clear(gc_context *gc);

// mark active objects
void gc_mark(lisp_context *lisp);
// recover unmarked objects.
void gc_recover_unmarked(gc_context *gc);

bool gc_mark_cons(gc_context *gc, cons *c);
bool cons_is_marked(gc_context *gc, cons *c);

bool vector_is_marked(void *vector);

lisp_value lisp_count_allocated();
void *gc_clone(const void *mem, size_t s);
void *nogc_clone(const void *mem, size_t s);
// functions
lisp_value lisp_eval(lisp_scope *scope, lisp_value value);
lisp_value lisp_eval_lisp_string(lisp_value string);
// eval: (a1)
lisp_value lisp_eval1(lisp_scope *scope, lisp_value a1);
// eval: (a1 (quote a2))
lisp_value lisp_eval_quoted2(lisp_scope *scope, lisp_value a1, lisp_value a2);
lisp_value lisp_eval_progn(lisp_scope *scope, lisp_value body);

// lets add more of these:
lisp_value lisp_eval_function2(lisp_scope *scope, lisp_function *f,
                               lisp_value arg1, lisp_value arg2);

int64_t get_symbol_id(const char *s);
lisp_value print(lisp_value v);
lisp_value get_symbol(const char *s);
lisp_value println(lisp_value v);
lisp_value println_shallow(lisp_value v);
lisp_value lisp_value_to_string(lisp_value v);
lisp_value lisp_string_concat(lisp_value a, lisp_value b);
lisp_value lisp_length(lisp_value cons_list);
size_t list_length(lisp_value cons_list);
const char *lisp_type_to_string(lisp_type t);

lisp_value new_cons(lisp_value a, lisp_value b);
lisp_value copy_cons(lisp_value a);

lisp_value car(lisp_value v);
lisp_value cdr(lisp_value v);
lisp_value set_car(lisp_value cons, lisp_value new_car);
lisp_value set_cdr(lisp_value cons, lisp_value new_car);
lisp_value cadr(lisp_value v);
lisp_value cddr(lisp_value v);
lisp_value pop(lisp_value *v);
lisp_value list_append(lisp_value a, lisp_value b);
void *lisp_malloc(size_t s);
void *lisp_realloc(void *p, size_t s);
void lisp_free(void *p);
lisp_value lisp_trace_allocations(lisp_value c);
lisp_value lisp_get_allocated();
gc_context *gc_context_new();
lisp_value lisp_code_location(lisp_value cons);
lisp_value lisp_print_code_location(lisp_value cons);
lisp_value lisp_print_scope(lisp_value scope);
void lisp_print_scope2(lisp_scope *s);

lisp_value lisp_string(const char *str);

lisp_value lisp_make_hashtable(lisp_value *args, int n);
lisp_value lisp_make_hashtable0();
lisp_value lisp_hashtable_set(lisp_value ht, lisp_value key, lisp_value value);
lisp_value lisp_hashtable_get(lisp_value _ht, lisp_value key);
lisp_value lisp_hashtable_count(lisp_value ht);
lisp_value lisp_hashtable_keys(lisp_value _ht);
lisp_value lisp_hashtable_values(lisp_value _ht);
// void * lisp_realloc(void * p, size_t v);

lisp_value get_symbol(const char *s);
lisp_value get_symbol_cached(lisp_value *v, const char *s);
bool eq(lisp_value a, lisp_value b);
const char *symbol_name(int64_t id);

lisp_context *lisp_context_new();
// mostly for testing. creates a context without any content.
lisp_context *lisp_context_new_bare();

lisp_value make_vector(lisp_value len, lisp_value _default);
lisp_value make_native_vector(lisp_value len, lisp_value _default);
lisp_value vector_length(lisp_value v);
lisp_value vector_ref(lisp_value _vector, lisp_value index);
lisp_value vector_ref_2(lisp_vector *vector, int index);
lisp_value vector_set(lisp_value vector, lisp_value index, lisp_value v);
lisp_value vector_elem_type(lisp_value vector);
lisp_value vector_copy(lisp_value vector);
lisp_value vector_resize(lisp_value vector, lisp_value elem_count);

void *vector_data_pointer(lisp_value vector);
lisp_type vector_element_type(lisp_value vector);

lisp_value integer(int64_t v);
lisp_value rational(double v);
lisp_value float32(float v);
double as_rational(lisp_value v);

lisp_value byte(unsigned char v);
lisp_value native_pointer(void *ptr);
lisp_value lisp_rational(lisp_value value);
lisp_value lisp_float32(lisp_value value);

size_t lisp_type_size(lisp_type type);
lisp_value lisp_type_of(lisp_value v);

bool eq(lisp_value a, lisp_value b);
bool is_nil(lisp_value a);
bool is_list(lisp_value a);
bool is_vector(lisp_value a);
bool is_function(lisp_value v);

lisp_value lisp_is_symbol(lisp_value a);
lisp_value lisp_is_list(lisp_value a);
lisp_value lisp_error(lisp_value v);
bool lisp_error_state();

void raise_string(const char *str);
bool type_assert(lisp_value val, lisp_type type);
bool type_assert_extra(lisp_value val, lisp_type type, const char * variable);
bool elem_type_assert(lisp_value vector, lisp_type type);

lisp_scope *lisp_scope_new(lisp_scope *super);
void lisp_scope_stack(lisp_scope *stack_scope, lisp_scope *super,
                      cons *argsbuffer, size_t cnt);
bool lisp_scope_try_get_value(lisp_scope *scope, lisp_value sym,
                              lisp_value *out);
lisp_value lisp_scope_get_value(lisp_scope *scope, lisp_value sym);
lisp_value lisp_scope_set_value(lisp_scope *scope, lisp_value sym,
                                lisp_value value);
lisp_value lisp_scope_create_value(lisp_scope *scope, lisp_value sym,
                                   lisp_value value);
lisp_scope *lisp_context_get_root_scope();
lisp_scope *lisp_get_root_scope();

void lisp_register_value(const char *name, lisp_value value);
void lisp_register_native(const char *name, int nargs, void *fptr);
void lisp_register_native_noeval(const char *name, int nargs, void *fptr);
void lisp_register_native_macrolike(const char *name, lisp_value (*fptr)(lisp_scope*,lisp_value));

lisp_value lisp_read_string(const char *str);

void lrn(const char *l, int args, void *f);


//lisp_value lisp_eval_stream2(lisp_scope * scope, lisp_value (* next)(void *data), void * data);
// macros

#define cdddr(x) cdr(cddr(x))
#define cddddr(x) cdr(cdddr(x))
#define cdddddr(x) cdr(cddddr(x))
#define cddddddr(x) cdr(cdddddr(x))
#define cdddddddr(x) cdr(cddddddr(x))

#define caar(x) car(car(x))
#define cdar(x) cdr(car(x))
#define caddr(x) car(cddr(x))
#define cadddr(x) car(cdddr(x))
#define caddddr(x) car(cddddr(x))
#define cadddddr(x) car(cdddddr(x))
#define caddddddr(x) car(cddddddr(x))
#define cadddddddr(x) car(cdddddddr(x))

#define TYPE_ASSERT(v, t)                                                      \
  if (!type_assert_extra(v, t, #v))                                                      \
    return nil;
#define TYPE_ASSERT2(v, t) ASSERT(type_assert(v, t));
#define RETURN_ERROR(err)                                                      \
  {                                                                            \
    lisp_error(err);                                                           \
    return nil;                                                                \
  }
#define RAISE(err)                                                             \
  {                                                                            \
    raise_string(err);                                                         \
    return nil;                                                                \
  }
#define EXPR_ASSERT(expr)                                                      \
  if (!(expr)) {                                                               \
    RAISE("AssertError: '" #expr "'"                                           \
          " returned false");                                                  \
  }
#define CHECK_ERROR()                                                          \
  if (lisp_error_state())                                                      \
    return nil;
extern bool gc_unsafe_stack;
