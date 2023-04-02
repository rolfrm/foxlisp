#include <iron/full.h>
#include <microio.h>
#include "foxlisp.h"
#include "foxlisp_internal.h"

lisp_value make_vector(lisp_value len, lisp_value _default) {
  TYPE_ASSERT(len, LISP_INTEGER);
  size_t l = lisp_value_integer(len);
  size_t elem_size = lisp_type_size(_default.type);
  
  void *data = lisp_malloc(l * elem_size);

  lisp_vector *vector = lisp_malloc(sizeof(*vector));
  vector->data = data;
  vector->count = l;
  vector->elem_size = elem_size;
  vector->default_value = _default;
  lisp_value v = vector_lisp_value(vector);
  for (size_t i = 0; i < l; i++)
    vector_set(v, integer_lisp_value(i), _default);

  return v;
}

lisp_value make_native_vector(lisp_value len, lisp_value _default) {
  TYPE_ASSERT(len, LISP_INTEGER);
  size_t l = lisp_value_integer(len);
  size_t elem_size = lisp_type_size(_default.type);
  
  void *data = alloc0(l * elem_size);

  lisp_vector *vector = lisp_malloc(sizeof(*vector));
  vector->data = data;
  vector->count = l;
  vector->elem_size = elem_size;
  vector->default_value = _default;
  lisp_value v = native_vector_lisp_value(vector);
  for (size_t i = 0; i < l; i++)
    vector_set(v, integer_lisp_value(i), _default);

  return v;
}

lisp_value vector_length(lisp_value v) {
  EXPR_ASSERT(is_vector(v));
  return integer(lisp_value_vector(v)->count);
}

lisp_value vector_ref_2(lisp_vector *vector, int i) {
  var v = vector->default_value;
  void *src = vector->data + i * vector->elem_size;
  void *dst;
  if (is_nil(v)) {
    return ((lisp_value *)vector->data)[i];
  } else if (is_float32(v)) {
    v.rational = (double)((float *)vector->data)[i];
    return v;
  } else
    dst = &v.integer;
  memcpy(dst, src, vector->elem_size);

  return v;
}

lisp_value vector_ref(lisp_value _vector, lisp_value k) {
  EXPR_ASSERT(is_vector(_vector));
  TYPE_ASSERT(k, LISP_INTEGER);
  var vector = lisp_value_vector(_vector);
  var i = lisp_value_integer(k);
  return vector_ref_2(vector, i);
}

lisp_value vector_set(lisp_value _vector, lisp_value k, lisp_value value) {
  EXPR_ASSERT(is_vector(_vector));
  TYPE_ASSERT(k, LISP_INTEGER);
  var i = (size_t)lisp_value_integer(k);
  var vector = lisp_value_vector(_vector);
  var v = value;
  void *dst = vector->data + i * vector->elem_size;
  if (i >= vector->count) {
    raise_string("index outside of the bounds of the vector\n");
    return nil;
  }
  void *src;
  if (is_nil(vector->default_value))
    ((lisp_value *)vector->data)[i] = value;
  else {
    TYPE_ASSERT(value, vector->default_value.type);
    src = &v.integer;
    if (is_float32(vector->default_value)) {
      ((float *)src)[0] = v.rational;
    }

    memcpy(dst, src, vector->elem_size);
  }
  return nil;
}

static lisp_value vector_resize(lisp_value vector, lisp_value k) {
  let type = lisp_value_type(vector);
  EXPR_ASSERT(type == LISP_VECTOR || type == LISP_NATIVE_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  bool is_native = type == LISP_NATIVE_VECTOR;
  size_t l = (size_t)lisp_value_integer(k);
  var vec0 = lisp_value_vector(vector);
  size_t elem_size = lisp_type_size(lisp_value_type(vec0->default_value));
  
  if(is_native){
    vec0->data = realloc(vec0->data, elem_size * l);
    ssize_t l2 = l - vec0->count;
    if(l2 > 0)
      memset(vec0->data + vec0->count * elem_size, 0, l2);
    vec0->count = l;
    return vector;
  }
  lisp_vector *vec = lisp_malloc(sizeof(*vec));

  void *new_data = lisp_malloc(l * elem_size);
  size_t prevCount = MIN(l, vec->count);
  memcpy(new_data, vec->data, prevCount * elem_size);
  vec->data = new_data;
  vec->count = l;

  return vector_lisp_value(vec);
}

lisp_value vector_elem_type(lisp_value vector) {
  TYPE_ASSERT(vector, LISP_VECTOR);
  return lisp_type_of(lisp_value_vector(vector)->default_value);
}

lisp_value vector_copy(lisp_value vector) {
  TYPE_ASSERT(vector, LISP_VECTOR);
  var vec = lisp_value_vector(vector);
  let vector2 = make_vector(integer_lisp_value(vec->count), vec->default_value);
  var vec2 = lisp_value_vector(vector2);
  memcpy(vec2->data, vec->data, vec2->count * vec2->elem_size);
  return vector2;
}

static lisp_value vector_to_string(lisp_value vector) {
  vector = vector_copy(vector);
  var dst = lisp_value_vector(vector);
  dst->count = dst->count * dst->elem_size;
  dst->default_value = byte(0);
  var cnt = dst->count;
  u8 *ptr = dst->data;
  size_t i;
  for (i = 0; i < cnt; i++) {
    if (ptr[i] == 0)
      break;
  }
  if (i == cnt)
    i = i + 1;
  vector = vector_resize(vector, integer_lisp_value(i));

  return string_lisp_value(dst->data);
}

static lisp_value string_to_vector(lisp_value str) {
  TYPE_ASSERT(str, LISP_STRING);
  char *strbuf = lisp_value_string(str);
  size_t l = strlen(strbuf) + 1;
  size_t elem_size = 1;

  lisp_vector *vector = lisp_malloc(sizeof(*vector));
  vector->data = strbuf;
  vector->count = l - 1;
  vector->elem_size = elem_size;
  vector->default_value = byte(0);
  return vector_lisp_value(vector);
}

static lisp_value vector_native_element_pointer(lisp_value vector,
                                                lisp_value k) {
  TYPE_ASSERT(vector, LISP_VECTOR);
  TYPE_ASSERT(k, LISP_INTEGER);
  var vec = lisp_value_vector(vector);
  void *ptr = vec->data + lisp_value_integer(k) * vec->elem_size;
  return native_pointer_lisp_value(ptr);
}

void *vector_data_pointer(lisp_value vector) {
  TYPE_ASSERT2(vector, LISP_VECTOR);
  var vec = lisp_value_vector(vector);
  void *ptr = vec->data;
  return ptr;
}

lisp_value lisp_is_vector(lisp_value value){
  return is_vector(value) ? t : nil;
}

void load_vector_module() {
  lisp_register_native("make-vector", 2, make_vector);
  lisp_register_native("make-native-vector", 2, make_native_vector);
  lisp_register_native("vector-element-type", 1, vector_elem_type);
  lisp_register_native("vector-native-element-pointer", 2,
                       vector_native_element_pointer);
  lisp_register_native("vector-resize", 2, vector_resize);
  lisp_register_native("vector->string", 1, vector_to_string);
  lisp_register_native("string->vector", 1, string_to_vector);
  lisp_register_native("vector?", 1, lisp_is_vector);
}
