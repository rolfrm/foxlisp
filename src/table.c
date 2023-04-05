#include <iron/full.h>
#include "microio.h"
#include "foxlisp.h"

static int i64_cmp(const i64 * k1, const i64 * k2){
  if(*k1 > *k2)
    return 1;
  else if(*k1 == *k2)
    return 0;
  else return -1;
}

void i64_finds(const i64 * column, const i64 * keys, ssize_t * indexes, size_t cnt, size_t rows){
  for(size_t i = 0; i < cnt; i++)
    indexes[i] = -1;
  const i64 * key_area = column;
  
  const i64 * start = key_area;
  const i64 * end = key_area + rows;
  for(size_t i = 0; i < cnt || indexes == NULL; i++){

    size_t size = end - start;
    const i64 * key_index = NULL;
    const i64 * key = keys + i;
    int startcmp = i64_cmp(key, start);
    if(startcmp < 0) continue;
    if(startcmp == 0)
      key_index = start;
    else if(i64_cmp(key, end - 1) > 0){
      return;
    }else
      //key_index =memmem(start,size,key,table->key_size);
      key_index = bsearch(key, start, size, sizeof(i64), (void *)i64_cmp);
	  
    if(key_index == NULL){
      if(indexes != NULL)
		  indexes[i] = 0;
    }else{
      if(indexes != NULL)
		  indexes[i] = (key_index - column);
      start = key_index + 1;
    }    
  }
}

// keys must be sorted
void i64_insert_index(const i64 * column, const i64 * keys, size_t * indexes, size_t cnt, size_t rows){
     if(cnt == 0) return;
     size_t j = 0;
     if(rows == 0 || column[rows - 1] < keys[0]){
       j = rows;
     }
     for(size_t i = 0; i < cnt; i++){
       while(j < rows && i64_cmp(column + j, keys + i) < 0){
         j++;
       }
       indexes[i] = j + i;
     }
}


lisp_value table_columns(lisp_value table){
  return vector_ref(cdr(table), integer(2));
}

lisp_value table_column_names(lisp_value table){
  return vector_ref(cdr(table), integer(1));
}

lisp_value table_key_column(lisp_value table){
  return vector_ref(table_columns(table), integer(0));
}

lisp_value table_clear(lisp_value table){
  var columns = table_columns(table);
  var column_count = vector_length(columns).integer;
  for(i64 i = 0; i < column_count; i++){
    vector_resize(vector_ref(columns, integer(i)), integer(0));
  }
  return nil;
}



size_t table_rows(lisp_value table){
  return table_key_column(table).vector->count;
}

lisp_value table_insert_index(lisp_value table, lisp_value key){
  //lisp_value table_data = cdr(table);
  lisp_value columns = table_columns(table);
  lisp_value key_column = vector_ref(columns, integer(0));
  if(is_nil(key_column))
    RAISE("Unable to locate key column");
  lisp_type t = vector_element_type(key_column);
  void * data_ptr = vector_data_pointer(key_column);
  ASSERT(data_ptr != NULL);
  if(t == LISP_INTEGER){
    size_t index = 0;
    i64_insert_index(data_ptr, &key.integer, &index, 1, key_column.vector->count);
    return integer(index);
  }
  RAISE("Invalid data type for insert.");
  return nil;  
}

lisp_value table_select_into2(lisp_scope * scope, lisp_value argforms){
  lisp_value key_table = lisp_eval(scope, car(argforms));
  lisp_value data_table = lisp_eval(scope, cadr(argforms));
  lisp_value output_table = lisp_eval(scope, caddr(argforms));
  lisp_value pred = cadddr(argforms);
  
  var keys = table_key_column(key_table);
  var keys2 = table_key_column(data_table);
  
  var key_rows = table_rows(key_table);
  var keys1_ptr = vector_data_pointer(keys);
  var keys2_ptr = vector_data_pointer(keys2);
  
  var out_columns = table_columns(output_table);
  var in_columns = table_columns(data_table);
  
  
  ssize_t indexes[key_rows];
  i64_finds(keys2_ptr, keys1_ptr, indexes, key_rows, keys2.vector->count);
  size_t new_rows = 0;
  if(! is_nil(pred)){
    var column_count = (size_t) vector_length(in_columns).integer;
    var column_names = table_column_names(data_table);
    lisp_scope iter_scope[1] = {0};
    cons args3[column_count];
  
    for(size_t i = 0; i < column_count; i++){
     args3[i].car = vector_ref(column_names, integer(i));
    }
    lisp_scope_stack(iter_scope, scope, args3, column_count);
  
    for(size_t i = 0; i < key_rows; i++){
     if(indexes[i] >= 0){
        for(size_t j = 0; j < column_count; j++){
          args3[j].cdr = vector_ref(vector_ref(in_columns, integer(j)), integer(indexes[i]));
        }
        if(is_nil(lisp_eval(iter_scope, pred))){
          indexes[i] = -1;
        }else{
          new_rows += 1;
        }
     }
    }
    
  }else{
    for(size_t i = 0; i < key_rows; i++){
     if(indexes[i] >= 0)
       new_rows += 1;
    }
  }
  
  var cc = vector_length(out_columns).integer;
  lisp_value columns[cc];
  lisp_value incolumns[cc];
  for(int i = 0; i < cc; i++){
    columns[i] = vector_ref(out_columns, integer(i));
    incolumns[i] = vector_ref(in_columns, integer(i));
    vector_resize(columns[i], integer(new_rows));
  }
  size_t l = 0;
  for(size_t i = 0; i < key_rows; i++){
    int j = indexes[i];
    if(j == -1){
      continue;
    }
    for(int k = 0; k < cc; k++){
      vector_set(columns[k], integer(l), vector_ref(incolumns[k], integer(j)));
    }
    l += 1;
  }
   
  return nil;
}

lisp_value table_print(lisp_value table){
   var rows = table_rows(table);
   var column = table_columns(table);
   var column_names = table_column_names(table);
   UNUSED(rows);
   UNUSED(column);
   UNUSED(column_names);
   
   return nil;
}

lisp_value table_iter(lisp_scope * scope, lisp_value argforms){
  var table = lisp_eval(scope, car(argforms));
  var columns = table_columns(table);
  var column_names = table_column_names(table);
  var column_count = (size_t) vector_length(columns).integer;
  
  var rows = table_rows(table);
  
  lisp_scope iter_scope[1] = {0};
  cons args3[column_count];
  
  for(size_t i = 0; i < column_count; i++){
    args3[i].car = vector_ref(column_names, integer(i));
  }
  lisp_scope_stack(iter_scope, scope, args3, column_count);
  lisp_value result = nil;
  for(size_t i = 0; i < rows; i++){
    for(size_t j = 0; j < column_count; j++){
      args3[j].cdr = vector_ref(vector_ref(columns, integer(j)), integer(i));
    }
    result = lisp_eval_progn(iter_scope, cdr(argforms));
    // update every column except from the key one .dont update the key column.
    for(size_t j = 1; j < column_count; j++){
      args3[j].cdr = vector_set(vector_ref(columns, integer(j)), integer(i), args3[j].cdr);
    }
  }
  
  return result;
}

void table_register() { 
  
  lisp_register_native_macrolike("table:iter", table_iter);
  lisp_register_native("table-insert-index", 2, table_insert_index); 
  lisp_register_native_macrolike("table:select", table_select_into2);
  
  lisp_register_native("table:clear", 1, table_clear);
}