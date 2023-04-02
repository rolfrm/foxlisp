#include <iron/full.h>
#include "microio.h"
#include "foxlisp.h"

typedef struct {
  lisp_value name;
  void **columns;
  size_t column_count;
  lisp_value *column_names;
  lisp_type *column_type;

} table;

lisp_value deftable(lisp_value *v, size_t count) {
  UNUSED(v);
  UNUSED(count);
  EXPR_ASSERT(count >= 1);
  for (size_t i = 0; i < count; i++) {
    println(v[i]);
  }

  lisp_value name = v[0];
  size_t column_count = count - 1;

  table tb = {0};
  tb.column_count = column_count;
  tb.name = name;
  
  

  return new_cons(get_symbol("table"),
                  native_pointer_lisp_value(iron_clone(&tb, sizeof(tb))));
}

void table_register() { lisp_register_native_noeval("deftable", -1, deftable); }