#include <iron/full.h>
#include "foxlisp.h"

static int lisp_value_hash(lisp_value v) {
  switch (lisp_value_type(v)) {
  case LISP_CONS: {
    cons *c = lisp_value_cons(v);
    int hash = lisp_value_hash(c->car);
    hash = hash ^ lisp_value_hash(c->cdr);
    hash *= 16777619;
    return hash;
  }
  default:;
  }
  int hash = (i32)2166136261;
  hash = hash ^ v.type;
  hash *= 16777619;
  hash = hash ^ v.integer;
  hash *= 16777619;
  return hash;
}

static int lisp_value_full_hash(const void *_key_data, void *userdata) {
  UNUSED(userdata);
  const lisp_value *key_data = _key_data;
  int h = lisp_value_hash(*key_data);
  return h;
}

static bool lisp_value_full_compare(const void *_k1, const void *_k2,
                                    void *userdata) {
  UNUSED(userdata);
  const lisp_value *k1 = _k1, *k2 = _k2;
  return equals(*k1, *k2);
}
void ht_init(hash_table *ht);

// TODO: add support for GCing >1 columns of the hashtable key.
lisp_value lisp_make_hashtable(lisp_value *args, int n) {
  static lisp_value full_keyword;
  static lisp_value weak_keyword;
  static lisp_value keys_keyword;
  bool full = false;
  bool weak = false;
  int keys = 1;
  for (int i = 0; i < n; i++) {
    lisp_value arg = args[i];
    if (eq(arg, get_symbol_cached(&full_keyword, ":deep-equality")))
      full = true;

    if (eq(arg, get_symbol_cached(&weak_keyword, ":weak")))
      weak = true;
    if (eq(arg, get_symbol_cached(&keys_keyword, ":keys"))) {
      if (i + 1 == n) {
        raise_string("invalid use of :keys");
        return nil;
      }
      keys = args[i + 1].integer;
      i += 1;
    }
  }

  hash_table *ht = lisp_malloc(sizeof(*ht));
  ht_create3(ht, 4, sizeof(lisp_value) * keys, sizeof(lisp_value));

  ht_set_alloc(ht, lisp_malloc, lisp_free);
  if (full) {
    ht->hash = lisp_value_full_hash;
    ht->compare = lisp_value_full_compare;
  }
  if (weak) {
    ((void **)&ht->userdata)[0] = (void *)(size_t)LISP_HASHTABLE_WEAK_KEYS;
  }
  ht_init(ht);
  return hashtable_lisp_value(ht);
}

lisp_value lisp_make_hashtable0() { return lisp_make_hashtable(NULL, 0); }

lisp_value lisp_make_hashtable_weak_keys() {
  hash_table *ht = lisp_malloc(sizeof(*ht));
  ht_create3(ht, 8, sizeof(lisp_value), sizeof(lisp_value));
  ht_set_alloc(ht, lisp_malloc, lisp_free);
  ((void **)&ht->userdata)[0] = (void *)(size_t)LISP_HASHTABLE_WEAK_KEYS;
  return hashtable_lisp_value(ht);
}

lisp_value lisp_hashtable_set(lisp_value _ht, lisp_value key,
                              lisp_value value) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  ht_set(ht, &key, &value);
  return nil;
}

lisp_value lisp_hashtable_get(lisp_value _ht, lisp_value key) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  lisp_value value;
  if (ht_get(ht, &key, &value))
    return value;
  return nil;
}

lisp_value lisp_hashtable_setn(lisp_value *values, size_t count) {
  if (count <= 2) {
    raise_string("not enough values for set");
    return nil;
  }

  var _ht = values[0];
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  ht_set(ht, values + 1, values + count - 1);
  return nil;
}

lisp_value lisp_hashtable_getn(lisp_value *values, size_t count) {
  if (count <= 1) {
    raise_string("not enough values for get");
    return nil;
  }

  lisp_value ht = values[0];
  lisp_value value = {0};

  if (ht_get(lisp_value_hashtable(ht), &values[1], &value))
    return value;
  return nil;
}

lisp_value lisp_hashtable_count(lisp_value ht) {
  TYPE_ASSERT(ht, LISP_HASHTABLE);
  var ht2 = lisp_value_hashtable(ht);
  return integer_lisp_value(ht2->count);
}

lisp_value lisp_hashtable_remove(lisp_value _ht, lisp_value key) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  if (ht_remove(ht, &key))
    return t;
  return nil;
}

lisp_value lisp_hashtable_get2(lisp_value _ht, lisp_value key) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  lisp_value value;
  if (ht_get(ht, &key, &value))
    return new_cons(t, value);
  return nil;
}

static void iter_add_key(void *key, void *elem, void *user_data) {
  UNUSED(elem);
  lisp_value *ud = user_data;
  *ud = new_cons(((lisp_value *)key)[0], *ud);
}

lisp_value lisp_hashtable_keys(lisp_value _ht) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  lisp_value keys = nil;
  ht_iterate(ht, iter_add_key, &keys);
  return keys;
}

static void iter_add_value(void *key, void *elem, void *user_data) {
  UNUSED(key);
  lisp_value *ud = user_data;
  *ud = new_cons(((lisp_value *)elem)[0], *ud);
}

lisp_value lisp_hashtable_values(lisp_value _ht) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  lisp_value keys = nil;
  ht_iterate(ht, iter_add_value, &keys);
  return keys;
}

lisp_value lisp_hashtable_clear(lisp_value _ht) {
  TYPE_ASSERT(_ht, LISP_HASHTABLE);
  hash_table *ht = lisp_value_hashtable(_ht);
  ht_clear(ht);
  return nil;
}

void load_hashtable_module() {

  lisp_register_native("make-hashtable", -1, lisp_make_hashtable);
  lisp_register_native("hashtable-ref", 2, lisp_hashtable_get);
  lisp_register_native("hashtable-set", 3, lisp_hashtable_set);
  lisp_register_native("hashtable-refn", -1, lisp_hashtable_getn);
  lisp_register_native("hashtable-setn!", -1, lisp_hashtable_setn);
  lisp_register_native("hashtable-remove", 2, lisp_hashtable_remove);
  lisp_register_native("hashtable-ref2", 2, lisp_hashtable_get2);
  lisp_register_native("hashtable-keys", 1, lisp_hashtable_keys);
  lisp_register_native("hashtable-values", 1, lisp_hashtable_values);
  lisp_register_native("hashtable-clear", 1, lisp_hashtable_clear);
  lisp_register_native("hashtable-count", 1, lisp_hashtable_count);
}
