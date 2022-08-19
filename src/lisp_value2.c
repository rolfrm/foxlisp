
static inline bool is_integer_type(lisp_type t){
  switch(t){
  case LISP_INTEGER:
  case LISP_BYTE:
  case LISP_INTEGER32:
    return true;
  default:
    break;
  }
  return false;  
}

inline bool is_float_type(lisp_type t){
  switch(t){
  case LISP_RATIONAL:
  case LISP_FLOAT32:
    return true;
  default:
    break;
  }
  return false;  
}

inline bool is_nil(lisp_value v){
  return v.type == LISP_NIL;
}

inline bool is_t(lisp_value v){
  return v.type == LISP_T;
}

inline bool is_cons(lisp_value v){
  return v.type == LISP_CONS;
}

inline bool is_integer(lisp_value v){
  return v.type == LISP_INTEGER;
}

inline bool is_float(lisp_value a){
  return a.type == LISP_RATIONAL || a.type == LISP_FLOAT32;
}

inline bool is_float32(lisp_value a){
  return a.type == LISP_FLOAT32;
}

inline bool is_string(lisp_value a){
  return a.type == LISP_STRING;
}

inline bool is_symbol(lisp_value a){
  return a.type == LISP_SYMBOL;
}

inline bool is_regular_symbol(lisp_value a){
  return is_symbol(a) && !is_keyword(a);
}

inline bool is_function(lisp_value a){
  return a.type == LISP_FUNCTION;
}

inline bool is_function_macro(lisp_value a){
  return a.type == LISP_FUNCTION_MACRO;
}

inline bool is_function_native(lisp_value a){
  return a.type == LISP_FUNCTION_NATIVE;
}

inline bool is_macro_builtin(lisp_value a){
  return a.type == LISP_MACRO_BUILTIN;
}

inline bool is_scope(lisp_value a){
  return a.type == LISP_SCOPE;
}

inline bool is_keyword(lisp_value sym){
  return is_symbol(sym) && lisp_value_symbol(sym) >= 0x100000;
}

inline f64 lisp_value_rational(lisp_value v){
  return v.rational;
}

inline lisp_value rational_lisp_value(double o){
  return (lisp_value){.type = LISP_RATIONAL, .rational = o};
}

inline int64_t lisp_value_as_integer(lisp_value v){
  if(is_float(v))
    return (int64_t)lisp_value_rational(v);
  return v.integer;
}

inline f64 lisp_value_as_rational(lisp_value v){
  if(is_float(v))
    return lisp_value_rational(v);
  return (f64) v.integer;
}

inline lisp_value string_lisp_value(const char * str){
  return (lisp_value){.type = LISP_STRING, .string = (char *) str};
}

inline char * lisp_value_string(lisp_value v){
  return v.string;
}

inline lisp_value integer_lisp_value(i64 i){
  return (lisp_value){.type = LISP_INTEGER, .integer = i};
}

inline int64_t lisp_value_integer(lisp_value v){
  return v.integer;
}

inline lisp_value byte_lisp_value(u8 i){
  return (lisp_value){.type = LISP_BYTE, .integer = i};
}

inline u8 lisp_value_byte(lisp_value v){
  return (u8)v.integer;
}

inline void * lisp_value_pointer(lisp_value val){
  return val.pointer;
}

inline lisp_vector * lisp_value_vector(lisp_value val){
  return lisp_value_pointer(val);
}

inline lisp_value vector_lisp_value(lisp_vector * vector){
  return (lisp_value){.type = LISP_VECTOR, .vector = vector};
}

inline lisp_type lisp_value_type(lisp_value val){
  return val.type;
}

inline lisp_value symbol_lisp_value(lisp_symbol sym){
  return (lisp_value){.type = LISP_SYMBOL, .symbol = sym };
}

inline lisp_symbol lisp_value_symbol(lisp_value val){
  return val.symbol; 
}

inline lisp_value cons_lisp_value(cons * cns){
  lisp_value ev2 = {.type = LISP_CONS, .cons = cns};
  return ev2;
}

inline cons * lisp_value_cons(lisp_value val){
  return val.cons;
}

inline lisp_value function_lisp_value(lisp_function * f){
  return (lisp_value){.type = LISP_FUNCTION, .function = f};
}

inline lisp_function * lisp_value_function(lisp_value val){
  return val.function;
}

lisp_native_function * lisp_value_native_function(lisp_value val){ return val.nfunction;}


inline lisp_value float32_lisp_value(f32 v){
  return (lisp_value){.type = LISP_FLOAT32, .rational = v };
}

inline lisp_value native_pointer_lisp_value(void * v){
  return (lisp_value){.type = LISP_NATIVE_POINTER, .pointer = v };
}

inline lisp_value allocated_pointer_lisp_value(void * ptr){
  return (lisp_value){.type = LISP_ALLOCATED_POINTER, .pointer = ptr };
}

inline lisp_value lisp_pointer_to_lisp_value(lisp_value * ptr){
  return (lisp_value){.type = LISP_NATIVE_POINTER_TO_VALUE, .pointer = ptr};                   
}

inline hash_table * lisp_value_hashtable(lisp_value v){
  return v.native_pointer;
}

inline lisp_value hashtable_lisp_value(hash_table * ht){
  return (lisp_value){.type = LISP_HASHTABLE, .native_pointer = ht};
}

inline lisp_value scope_lisp_value(lisp_scope * scope){
  return (lisp_value){.type = LISP_SCOPE, .pointer = scope };
}

inline lisp_scope * lisp_value_scope(lisp_value val){
  return val.pointer;
}


inline bool lisp_value_eq(lisp_value a, lisp_value b){
  if(lisp_value_type(a) != lisp_value_type(b)) return false;
  return a.integer == b.integer;
}

inline lisp_value bool_lisp_value(bool v){
  return v ? t : nil;
}


inline lisp_value global_index_lisp_value(size_t i){
  return (lisp_value){.type = LISP_GLOBAL_INDEX, .integer = i};
}

inline lisp_value local_index_lisp_value(size_t scope_level, size_t scope_index, bool scope_type){
  lisp_value new = {.type = LISP_LOCAL_INDEX,
    .local_index = {.scope_level = scope_level,
                      .scope_index = scope_index,
                      .scope_type = scope_type }};
  return new; 
}
