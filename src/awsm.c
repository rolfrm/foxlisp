#include <iron/full.h>
#include "microio.h"
#include "foxlisp.h"
//#include "../awsm/include/awsm.h"
//lisp_value awsm_new_module() {

  //var pointer = awsm_load_dynamic_module();
  //return new_cons(get_symbol("awsm-module"),
  //                native_pointer_lisp_value(pointer));
//}

lisp_value awsm_load_function_code(lisp_value fname, lisp_value buffer) {
  UNUSED(fname);
  UNUSED(buffer);
  return nil;
}

lisp_value awsm_function_eval(lisp_value module, lisp_value name) {
  UNUSED(module);
  UNUSED(name);
  return nil;
}

typedef struct {
  u8 *bytes;
  size_t count;
} byte_buffer;

void byte_buffer_write_raw(byte_buffer *buffer, void *data, size_t cnt) {
  buffer->bytes = realloc(buffer->bytes, buffer->count + cnt);
  memcpy(buffer->bytes + buffer->count, data, cnt);
  buffer->count += cnt;
}

bool symbol_name_eq(const char *str, lisp_value buf) {
  var name = symbol_name(buf.symbol);
  if (name == NULL)
    return false;
  if (strcmp(str, name) == 0)
    return true;
  return false;
}
lisp_value byte_buffer_new() {
  byte_buffer buffer = {0};
  return new_cons(
      get_symbol("byte-buffer"),
      native_pointer_lisp_value(iron_clone(&buffer, sizeof(buffer))));
}

lisp_value byte_buffer_print(lisp_value bb) {
  EXPR_ASSERT(symbol_name_eq("byte-buffer", car(bb)));
  TYPE_ASSERT(cdr(bb), LISP_NATIVE_POINTER);
  byte_buffer *bufptr = lisp_value_pointer(cdr(bb));
  for (size_t i = 0; i < bufptr->count; i++) {
    printf("%x", bufptr->bytes[i]);
  }
  return nil;
}

lisp_value byte_buffer_to_vector(lisp_value bb) {
  EXPR_ASSERT(symbol_name_eq("byte-buffer", car(bb)));
  TYPE_ASSERT(cdr(bb), LISP_NATIVE_POINTER);

  byte_buffer *bufptr = lisp_value_pointer(cdr(bb));

  lisp_value vec = make_vector(integer(bufptr->count), byte(0));
  ;
  u8 *d = vector_data_pointer(vec);
  for (size_t i = 0; i < bufptr->count; i++)
    d[i] = bufptr->bytes[i];

  return vec;
}

lisp_value byte_buffer_write(lisp_value *values, int count) {
  EXPR_ASSERT(count > 0);

  let buf = values[0];
  EXPR_ASSERT(symbol_name_eq("byte-buffer", car(buf)));
  TYPE_ASSERT(cdr(buf), LISP_NATIVE_POINTER);
  byte_buffer *bufptr = lisp_value_pointer(cdr(buf));

  for (int i = 1; i < count; i++) {
    var arg = values[i];
    switch (lisp_value_type(arg)) {
    case LISP_BYTE: {
      u8 b = (u8)arg.integer;
      byte_buffer_write_raw(bufptr, &b, 1);
    } break;
    case LISP_RATIONAL:
    case LISP_INTEGER:
      byte_buffer_write_raw(bufptr, &arg.integer, 8);
      break;
    case LISP_SYMBOL:
      if (is_keyword(arg)) {
        if (symbol_name_eq(":leb", arg)) {
          u8 data[16];

          io_writer w2 = {.data = data, .offset = 0, .size = 16};
          io_write_i64_leb(&w2, arg.integer);
          byte_buffer_write_raw(bufptr, data, w2.offset);
          i += 1;
          continue;
        }
      }
      raise_string("Invalid type for byte buffer write");
      return nil;
    default:
      raise_string("Invalid type for byte buffer write");
      return nil;
    }
  }

  return nil;
}

void awsm_register() {
  //lrn("awsm:new-module", 0, awsm_new_module);
  lrn("awsm:load-code", 2, awsm_load_function_code);
  lrn("byte-buffer-new", 0, byte_buffer_new);
  lrn("byte-buffer-write", -1, byte_buffer_write);
  lrn("byte-buffer-print", 1, byte_buffer_print);
  lrn("byte-buffer-to-vector", 1, byte_buffer_to_vector);
}
