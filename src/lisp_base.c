#include <iron/full.h>
#include "foxlisp.h"
#include <microio.h>

bool _string_eq(lisp_value a, lisp_value b) {
  var a_type = lisp_value_type(a);
  if (a_type != lisp_value_type(b))
    return false;
  if (a_type != LISP_STRING)
    return false;
  var as = lisp_value_string(a);
  var bs = lisp_value_string(b);
  if (strlen(as) != strlen(bs))
    return false;
  return strncmp(as, bs, strlen(as)) == 0;
}

lisp_value string_eq(lisp_value a, lisp_value b) {
  return bool_lisp_value(_string_eq(a, b));
}

lisp_value lisp_is_list(lisp_value a) {
  return bool_lisp_value(is_nil(a) || is_cons(a));
}

lisp_value lisp_is_cons(lisp_value a) { return bool_lisp_value(is_cons(a)); }

lisp_value lisp_length(lisp_value lst) {
  return integer_lisp_value(list_length(lst));
}

lisp_value lisp_is_symbol(lisp_value v) {
  if (is_symbol(v))
    return t;
  return nil;
}

lisp_value lisp_is_integer(lisp_value v) {
  if (is_integer(v))
    return t;
  return nil;
}

lisp_value lisp_is_rational(lisp_value v) {
  if (is_float(v))
    return t;
  return nil;
}

lisp_value lisp_is_number(lisp_value v) {
  if (is_float(v) || is_integer(v))
    return t;
  return nil;
}

static lisp_value lisp_is_scope(lisp_value v) {
  if (is_scope(v))
    return t;
  return nil;
}

static void normalize_numericals(lisp_value *values, int count) {
  static u8 numeric_tower[LISP_SCOPE];
  if (numeric_tower[0] == 0) {
    numeric_tower[LISP_NIL] = 0xFF;
    numeric_tower[LISP_T] = 0x0;
    numeric_tower[LISP_CONS] = 0xFF;
    numeric_tower[LISP_BYTE] = 1;
    numeric_tower[LISP_INTEGER32] = numeric_tower[LISP_BYTE] + 1;
    numeric_tower[LISP_INTEGER] = numeric_tower[LISP_INTEGER32] + 1;
    numeric_tower[LISP_FLOAT32] = numeric_tower[LISP_INTEGER] + 1;
    numeric_tower[LISP_RATIONAL] = numeric_tower[LISP_FLOAT32] + 1;
  }
  ASSERT(count > 0);
  lisp_type type = values[0].type;
  for (int i = 1; i < count; i++) {
    lisp_type thistype = LISP_NIL;
    if (is_integer(values[i])) {
      if (values[i].integer <= 0xFF) {
        thistype = LISP_BYTE;
      } else if (values[i].integer <= 0xFFFFFFFFL) {
        thistype = LISP_INTEGER32;
      } else
        thistype = LISP_INTEGER;
    } else if (values[i].type == LISP_FLOAT32) {
      thistype = LISP_FLOAT32;
    }
    if (values[i].type == LISP_RATIONAL) {
      thistype = LISP_RATIONAL;
    }
    if (numeric_tower[thistype] > numeric_tower[type])
      type = thistype;
  }
  if (is_float_type(type)) {
    for (int i = 0; i < count; i++) {
      if (is_integer(values[i]))
        values[i].rational = values[i].integer;
      values[i].type = type;
    }
  } else {
    for (int i = 0; i < count; i++) {
      if (is_float(values[i]))
        values[i].integer = (int64_t)values[i].rational;
      values[i].type = type;
    }
  }
}

#define lisp_procn2(values, count, ff, lf)                                     \
  if (count == 2 && values[0].type == values[1].type) {                        \
    if (is_float_type(values[0].type)) {                                       \
      double sum = values[0].rational;                                         \
      sum = ff(sum, values[1].rational);                                       \
      return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);    \
    } else {                                                                   \
      i64 sum = values[0].integer;                                             \
      sum = lf(sum, values[1].integer);                                        \
      return integer(sum);                                                     \
    }                                                                          \
  }                                                                            \
  normalize_numericals(values, count);                                         \
                                                                               \
  if (is_float(values[0])) {                                                   \
    double sum = values[0].rational;                                           \
    for (int i = 1; i < count; i++)                                            \
      sum = ff(sum, values[i].rational);                                       \
    return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);      \
  } else {                                                                     \
    i64 sum = values[0].integer;                                               \
    for (int i = 1; i < count; i++)                                            \
      sum = lf(sum, values[i].integer);                                        \
    return integer(sum);                                                       \
  }

static inline lisp_value lisp_procn(lisp_value *values, int count,
                                    void (*ff)(f64 *l, f64 *r),
                                    void (*lf)(i64 *l, i64 *r)) {
  if (count == 2 && values[0].type == values[1].type) {
    if (is_float_type(values[0].type)) {
      double sum = values[0].rational;
      ff(&sum, &(values[1]).rational);
      return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);
      ;
    } else {
      i64 sum = values[0].integer;
      lf(&sum, &(values[1]).integer);
      return integer(sum);
    }
  }
  normalize_numericals(values, count);

  if (is_float(values[0])) {
    double sum = values[0].rational;
    for (int i = 1; i < count; i++)
      ff(&sum, &(values[i]).rational);
    return values[0].type == LISP_FLOAT32 ? float32(sum) : rational(sum);
  } else {
    i64 sum = values[0].integer;
    for (int i = 1; i < count; i++)
      lf(&sum, &(values[i]).integer);
    return integer(sum);
  }
}

static inline lisp_value lisp_binn(lisp_value *values, int count,
                                   bool (*cmpf)(f64 l, f64 r),
                                   bool (*cmpi)(i64 l, i64 r)) {
  if (count == 2 && values[0].type == values[1].type) {
    if (is_float_type(values[0].type)) {
      return cmpf(values[0].rational, values[1].rational) ? t : nil;
    } else {
      return cmpi(values[0].integer, values[1].integer) ? t : nil;
    }
  }
  normalize_numericals(values, count);
  if (is_float(values[0])) {
    for (int i = 1; i < count; i++)
      if (!cmpf((values[i - 1]).rational, (values[i]).rational))
        return nil;
  } else {
    for (int i = 1; i < count; i++)
      if (!cmpi((values[i - 1]).integer, (values[i]).integer))
        return nil;
  }
  return t;
}

static void minimumf(f64 *a, f64 *b) { *a = MIN(*b, *a); }

static void minimumi(i64 *a, i64 *b) { *a = MIN(*b, *a); }

static lisp_value lisp_minimumn(lisp_value *values, int count) {
  return lisp_procn(values, count, minimumf, minimumi);
}

static void maximumf(f64 *a, f64 *b) { *a = MAX(*b, *a); }

static void maximumi(i64 *a, i64 *b) { *a = MAX(*b, *a); }

static lisp_value lisp_maximumn(lisp_value *values, int count) {
  return lisp_procn(values, count, maximumf, maximumi);
}

static bool lessf(f64 a, f64 b) { return a < b; }
static bool lessi(i64 a, i64 b) { return a < b; }

static lisp_value lisp_lessn(lisp_value *values, int count) {
  return lisp_binn(values, count, lessf, lessi);
}

static bool lesseqf(f64 a, f64 b) { return a <= b; }
static bool lesseqi(i64 a, i64 b) { return a <= b; }

static lisp_value lisp_lesseqn(lisp_value *values, int count) {
  return lisp_binn(values, count, lesseqf, lesseqi);
}

static bool greaterf(f64 a, f64 b) { return a > b; }
static bool greateri(i64 a, i64 b) { return a > b; }

static lisp_value lisp_greatern(lisp_value *values, int count) {
  return lisp_binn(values, count, greaterf, greateri);
}

static bool greatereqf(f64 a, f64 b) { return a >= b; }

static bool greatereqi(i64 a, i64 b) { return a >= b; }

static lisp_value lisp_greatereqn(lisp_value *values, int count) {
  return lisp_binn(values, count, greatereqf, greatereqi);
}

static lisp_value lisp_sin(lisp_value v) {
  return rational_lisp_value(sin(lisp_value_as_rational(v)));
}

static lisp_value lisp_cos(lisp_value v) {
  return rational_lisp_value(cos(lisp_value_as_rational(v)));
}
static lisp_value lisp_abs(lisp_value v) {
  return rational_lisp_value(fabs(lisp_value_as_rational(v)));
}

static lisp_value lisp_mod(lisp_value a, lisp_value b) {
  if (is_float(a) || is_float(b))
    return rational_lisp_value(
        fmod(lisp_value_as_rational(a), lisp_value_as_rational(b)));
  return integer_lisp_value(lisp_value_as_integer(a) %
                            lisp_value_as_integer(b));
}

static lisp_value parse_hex(lisp_value str) {
  TYPE_ASSERT(str, LISP_STRING);
  char *tp = NULL;
  int64_t o = strtoll(str.string, &tp, 16);
  if (tp != lisp_value_string(str))
    return integer_lisp_value(o);
  return nil;
}

static lisp_value hex_string(lisp_value i, lisp_value dec) {
  var v = lisp_value_integer(i);
  var l = lisp_value_integer(dec);
  int cnt = snprintf(NULL, 0, "%x", (u32)v);
  if (is_nil(dec))
    l = cnt;
  char *buf = lisp_malloc(l + 1);
  snprintf(buf + MAX(0, l - cnt), MIN(cnt, l) + 1, "%x", (u32)v);
  buf[l] = 0;
  for (int i = 0; i < MAX(0, l - cnt); i++)
    buf[i] = '0';
  return string_lisp_value(buf);
}

static lisp_value string_to_symbol(lisp_value string) {
  TYPE_ASSERT(string, LISP_STRING);
  return get_symbol(lisp_value_string(string));
}

static lisp_value symbol_to_string(lisp_value sym) {
  TYPE_ASSERT(sym, LISP_SYMBOL);
  char *sym_string = (char *)symbol_name(lisp_value_symbol(sym));
  return string_lisp_value(sym_string);
}

static lisp_value lisp_neqn(lisp_value *values, int count) {
  for (int i = 0; i < count; i++)
    for (int j = i + 1; j < count; j++)
      if (lisp_value_eq(values[i], values[j]))
        return nil;
  return t;
}

static lisp_value lisp_equals(lisp_value *values, int n) {
  if (n == 0)
    return nil;
  for (int i = 1; i < n; i++) {
    if (!equals(values[i - 1], values[i]))
      return nil;
  }
  return t;
}

lisp_value copy_cons(lisp_value a) {
  if (is_cons(a))
    return new_cons(car(a), copy_cons(cdr(a)));
  return a;
}

lisp_value copy_cons_deep(lisp_value a) {
  if (is_cons(a))
    return new_cons(copy_cons(car(a)), copy_cons(cdr(a)));
  return a;
}

static lisp_value string_starts_with(lisp_value str, lisp_value str2) {
  TYPE_ASSERT(str, LISP_STRING);
  TYPE_ASSERT(str2, LISP_STRING);
  var astr = lisp_value_string(str);
  var bstr = lisp_value_string(str2);
  return bool_lisp_value(strncmp(astr, bstr, strlen(bstr)) == 0);
}

lisp_value _list_append(lisp_value v1, lisp_value v2) {
  if (is_nil(v1))
    return v2;
  return new_cons(car(v1), _list_append(cdr(v1), v2));
}

lisp_value list_append(lisp_value v1, lisp_value v2) {
  if (is_nil(v2))
    return v1;
  return _list_append(v1, v2);
}

void load_lisp_base() {
  lisp_register_native("string=", 2, string_eq);
  lisp_register_native("cons?", 1, lisp_is_cons);
  lisp_register_native("integer?", 1, lisp_is_integer);
  lisp_register_native("number?", 1, lisp_is_number);
  lisp_register_native("rational?", 1, lisp_is_rational);
  lisp_register_native("scope?", 1, lisp_is_scope);
  lisp_register_native("<", -1, lisp_lessn);
  lisp_register_native(">", -1, lisp_greatern);
  lisp_register_native("<=", -1, lisp_lesseqn);
  lisp_register_native(">=", -1, lisp_greatereqn);
  lisp_register_native("/=", -1, lisp_neqn);
  lisp_register_native("equal?", -1, lisp_equals);
  lisp_register_native("min", -1, lisp_minimumn);
  lisp_register_native("max", -1, lisp_maximumn);
  lisp_register_native("sin", 1, lisp_sin);
  lisp_register_native("cos", 1, lisp_cos);
  lisp_register_native("abs", 1, lisp_abs);
  lisp_register_native("mod", 2, lisp_mod);
  lisp_register_native("parse-hex", 1, parse_hex);
  lisp_register_native("hex-string", 2, hex_string);
  lisp_register_native("symbol->string", 1, symbol_to_string);
  lisp_register_native("string->symbol", 1, string_to_symbol);
  lisp_register_native("copy-list", 1, copy_cons);
  lisp_register_native("copy-list-deep", 1, copy_cons_deep);
  lisp_register_native("string-starts-with", 2, string_starts_with);
}
