#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "utils.h"
typedef int8_t i8;

typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

void * (* ht_mem_malloc)(size_t s);
void (* ht_mem_free)(void *);
static void * alloc(size_t s){
  void * d;
  if(ht_mem_malloc != NULL) d = ht_mem_malloc(s);
  else d = malloc(s);
  memset(d, 0, s);
  return d;
}
static void dealloc(void * d){
  if(ht_mem_free != NULL) ht_mem_free(d);
  else free(d);
}

u32 djb2_hash(const char  * str, size_t count)
{
  u32 hash = 5381;
  for(size_t i = 0; i < count; i++){
    u8 c = str[i];
    hash = ((hash << 5) + hash) + c;
  }
  return hash;
}

u64 fnv1a_hash(const char * data, size_t count){
  u64 bias = 0xcbf29ce484222325L;
  u64 prime = 0x100000001b3;
  u64 hash = bias;
  for(size_t i = 0; i < count; i++){
    hash ^= data[i];
    hash *=  prime;
  }
  return hash;
}

u64 hyper_hash(const void * data, size_t count){
  u64 bias = 0xcbf29ce484222325UL;
  u64 prime1 = 0x100000001b3;
  u64 prime2 = 18446744073709551557UL;
  u64 hash = bias;
  u64 h = 0;
  u64 * d = (u64 *) data;
  u64 c = count / 8;
  size_t rest = count - c * 8;
  if(rest > 0){
    memcpy(&h, data + c * 8, rest);
  }
  size_t i = 0;
  for(; i < c; i++){
    hash ^= d[i];
    hash *= prime1;
    hash *= prime2;
  }
  hash ^= rest;
  hash *= prime1;
  hash *= prime2;
  
  return hash;
}


u32 fnv1a_hash2(const char * data, size_t count){
  return (u32)fnv1a_hash(data, count);
}

u32 hash1(const char * data, size_t count){
  return fnv1a_hash2(data, count);
  //return djb2_hash(data, count);
  //return hyper_hash(data, count);
}

typedef enum {
	      HT_FREE = 0,
	      HT_OCCUPIED = 1
}ht_state;

typedef bool (* compare_t ) (const void * k1, const void * k2, void * userdata);

typedef struct _hash_table hash_table;
struct _hash_table{
  // this is allowed to be null
  i32 (* hash )(const void * key_data, void * userdata);
  // this is allowed to be null
  void * userdata;
  // this is allowed to be null
  compare_t compare;
  
  void * keys;
  void * elems;
  ht_state * occupied;

  size_t capacity;
  size_t count;
  
  size_t key_size;
  size_t elem_size;
};

void ht_set_hash(hash_table * ht, i32 (* hash)(const void * key,  void * userdata)){
  ht->hash = hash;
}

void ht_set_compare(hash_table * ht, bool (* compare)(const void * k1, const void * k2, void * userdata)){
  ht->compare = compare;
}

i32 default_hash(const void * data, size_t size, void * userdata){
  UNUSED(userdata);
  return (i32) hash1(data, size);
}

bool default_compare(const void * key_a, const void * key_b, size_t size, void * userdata){
  UNUSED(userdata);
  return 0 == memcmp(key_a, key_b, size);
}

hash_table * ht_create2(size_t capacity, size_t key_size, size_t elem_size){
  hash_table * ht = alloc(sizeof(*ht));
  ht->capacity = capacity;
  ht->key_size = key_size;
  ht->elem_size = elem_size;
  ht->userdata = ht;
  ht->keys = alloc(capacity * key_size);
  ht->elems = alloc(capacity * elem_size);
  ht->occupied = alloc(capacity * sizeof(ht_state));
  return ht;
}

hash_table * ht_create(size_t key_size, size_t elem_size){
  return ht_create2(8, key_size, elem_size);
}


void ht_free(hash_table *ht){
  dealloc(ht->keys);
  dealloc(ht->elems);
  dealloc(ht->occupied);
  memset(ht, 0, sizeof(ht[0]));
  dealloc(ht);
}

bool ht_set(hash_table * ht, const void * key, const void * nelem);

static void ht_grow(hash_table * ht){
  var ht2 = ht_create2(ht->capacity * 2, ht->key_size, ht->elem_size);

  ht2->hash = ht->hash;
  ht2->compare = ht->compare;
  ht2->userdata = ht->userdata;
  for(u32 i = 0; i < ht->capacity; i++){
    if(ht->occupied[i] == HT_OCCUPIED){
      ht_set(ht2, ht->keys + i * ht->key_size, ht->elems + i * ht->elem_size);
    }
  }
  SWAP(*ht2, *ht);
  ht_free(ht2);
}

static i64 ht_find_free(const hash_table * ht, const void * key){
  size_t key_size = ht->key_size;
  i32 hash = ht->hash == NULL ? (i32)hash1(key, key_size) : ht->hash(key, ht->userdata);
  size_t capacity = ht->capacity;
  compare_t compare = ht->compare;

  // find a free slot using linear probing.
  for(size_t _i = 0, i = hash % ht->capacity; _i < capacity; _i++, i++){
    if(i >= capacity) i = 0;
    
    switch(ht->occupied[i]){
    case HT_FREE:
      return i;
    case HT_OCCUPIED:
      {
	let thiskey = ht->keys + i * key_size;
	if(compare != NULL && compare(key, thiskey, ht->userdata)){
	  return i;
	}else if(memcmp(thiskey, key, key_size) == 0){
	  return i;
	}	
      }
    }
  }
  return -1;	 
}

bool ht_set(hash_table * ht, const void * key, const void * nelem){
  if(ht->count * 2 > ht->capacity)
    ht_grow(ht);
  i64 index = ht_find_free(ht, key);
  
  size_t elem_size = ht->elem_size;
  size_t key_size = ht->key_size;
  memmove(ht->elems + index * elem_size, nelem, elem_size);
  // an unoccupied index was found.
  if(ht->occupied[index] == HT_FREE){
    ht->count += 1;
    memmove(ht->keys + index * key_size, key, key_size);
    ht->occupied[index] = HT_OCCUPIED;
    return true;
  }
  return false;
}

bool ht_get(hash_table * ht, const void *key, void * out_elem){
  i64 index = ht_find_free(ht, key);
  if(ht->occupied[index] == HT_FREE || index == -1)
    return false;
  size_t elem_size = ht->elem_size;
  if(out_elem != NULL)
    memcpy(out_elem, ht->elems + index * elem_size, elem_size);
  return true;
}

void _ht_remove_at(hash_table * ht, size_t index){
  ht->count -= 1;
  size_t key_size = ht->key_size;
  size_t elem_size = ht->elem_size;
  size_t capacity = ht->capacity;
  
 start:
  ht->occupied[index] = HT_FREE;
  for(size_t _i = 1, i = index + 1; _i < capacity; _i++, i++){
    // probe to find an element that can be moved back.
    if(i >= capacity) i = 0;
    if(ht->occupied[i] == HT_FREE)
      break;
    
    size_t thiskey = ht->keys + i * key_size;
    int hash = ht->hash == NULL ? (i32)hash1(thiskey, key_size) : ht->hash(thiskey, ht->userdata);
    size_t h2 = (size_t)(hash % capacity);
    if(h2 <= index){
      
      ht->occupied[index] = ht->occupied[i];
      memcpy(ht->keys + index * key_size, ht->keys + i * key_size, key_size);
      memcpy(ht->elems + index * elem_size, ht->elems + i * elem_size, elem_size);

      // set index to i and start another iteration.
      index = i;
      goto start;
    } 
  }
}


i64 _ht_remove(hash_table * ht, const void * key){
  i64 index = ht_find_free(ht, key);
  if(ht->occupied[index] == HT_FREE)
    return -1;
  _ht_remove_at(ht, index);
  return index;
}

bool ht_remove(hash_table * ht, const void * key){
  i64 index =_ht_remove(ht, key);
  return index != -1;
}

bool ht_remove2(hash_table * ht, const void * key, void * out_key, void * out_elem){
  i64 index = ht_find_free(ht, key);
  if(index == -1) return false;
  
  if(out_key)
    memcpy(out_key, ht->keys + index * ht->key_size, ht->key_size); 
  if(out_elem)
    memcpy(out_elem, ht->elems + index * ht->elem_size, ht->elem_size);
  _ht_remove_at(ht, index);
  return true;
}

void ht_clear(hash_table * ht){
  for(u32 i = 0; i < ht->capacity; i++)
    ht->occupied[i] = HT_FREE;
}

void ht_iterate(hash_table * ht, void (* it)(void * key, void * elem, void * user_data), void * userdata){
  size_t key_size = ht->key_size;
  size_t elem_size = ht->elem_size;
  for(size_t i = 0; i < ht->capacity; i++){
    if(ht->occupied[i] == HT_OCCUPIED){
      it(ht->keys + i * key_size, ht->elems + i * elem_size, userdata);
    }
  }
}

static i32 string_hash(const char ** key, void * userdata){
  UNUSED(userdata);
  u32 h = hash1(*key, strlen(*key));
  return (i32)h;
}

static bool string_compare(const char ** key1, const char ** key2, void * userdata){
  UNUSED(userdata);
  return strcmp(*key1, *key2) == 0;
}

hash_table * ht_create_strkey(size_t elem_size){
  hash_table * ht = ht_create(sizeof(char *), elem_size);
  ht_set_hash(ht, (i32 (*)(const void * a, void * userdata))string_hash);
  ht_set_compare(ht, (bool (*)(const void * a, const void * b, void * userdata))string_compare);
  return ht;
}
