#include <stdlib.h>
#include <stdint.h>
#include <microio.h>
#include <iron/full.h>
#include "foxlisp.h"

void skip_comment_and_whitespace(io_reader * rd){
  while(true){
	 uint8_t c = io_peek_u8(rd);
	 if(c == ' ' || c == '\n' || c == '\t'){
		io_read_u8(rd);
	 }
	 else if(c == ';'){
		while(true){
		  c = io_read_u8(rd);
		  if( c == '\n')
			 break;
		  if(c == 0)
			 break;
		}
	 }else{
		break;
	 }
  }
}

lisp_value read_token_string(io_reader * rd){
  
  io_writer wd = {0};
  io_read_u8(rd); // skip first quote.
  while(true){
	 uint8_t c = io_read_u8(rd);
    if(c == '\\'){
      var c2 = io_peek_u8(rd);
      if(c2 == 'n'){
        io_read_u8(rd);
        c = '\n';
      }else{
        c = io_read_u8(rd);
      }
    }else if(c == '"'){
		c = io_peek_u8(rd);
		if(c == '"'){
        io_read_u8(rd);
        // double quote
		  io_write_u8(&wd, c);
		}else{
		  break;
		}
	 }
	 else  if(c == 0)
		break;// incomplete string.
	 io_write_u8(&wd, c);
  }
  io_write_u8(&wd, 0);
  lisp_value v = string_lisp_value(gc_clone(wd.data, strlen(wd.data) + 1));
  io_writer_clear(&wd);
  return v;
}

lisp_value parse_token(const char * x, int count){

  char * tp = NULL;

  {
	 int64_t o = strtoll(x, &tp, 10);
	 if(tp == x + count)
		return integer_lisp_value(o);
  }
  
  {
	 double o = strtold(x, &tp);
	 if(tp == x + count)
		return rational_lisp_value(o);
  }
  
  // otherwise it is a symbol
  return symbol_lisp_value(get_symbol_id(x));
}


lisp_value read_token_data(io_reader * rd){
  uint8_t c = io_peek_u8(rd);
  if(c == '"'){
	 return read_token_string(rd);
  }
  io_writer wd = {0};
  while(true){
	 c = io_peek_u8(rd);
	 if(c == ' ' || c == ')' || c == '(' || c == '\t' || c == 0 || c == '\n'){
		break;
	 }
	 io_read_u8(rd);
	 io_write_u8(&wd, c);
  }
  io_write_u8(&wd, 0);
  lisp_value vv = parse_token(gc_clone(wd.data, wd.offset), wd.offset - 1);
  io_writer_clear(&wd);
  return vv;
}

lisp_value tokenize_stream(io_reader * rd){
  skip_comment_and_whitespace(rd);
  uint8_t c = io_peek_u8(rd);
  if(c == 0) return nil;
  if(c == ':'){
    var symbol = read_token_data(rd);
    return symbol;
  }
  if(c == '\''){
	 io_read_u8(rd);
  	 var c = new_cons(quote_sym, new_cons(tokenize_stream(rd), nil));
    return c;
  }
  if(c == '`'){
	 io_read_u8(rd);
	 return new_cons(quasiquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == ','){
	 io_read_u8(rd);
	 if(io_peek_u8(rd) == '@'){
		io_read_u8(rd);
		return new_cons(unquote_splice_sym, new_cons(tokenize_stream(rd), nil));
	 }
	 return new_cons(unquote_sym, new_cons(tokenize_stream(rd), nil));
  }
  if(c == '('){
	 io_read_u8(rd);

	 skip_comment_and_whitespace(rd);
	 if(io_peek_u8(rd) == ')'){
		io_read_u8(rd);
		return nil;
	 }
	 lisp_value head = nil;
	 var next = head;
	 while(true){
		var v = tokenize_stream(rd);
		var new = new_cons(v, nil);
		if(is_nil(head)){
		  head = new;
		}else{
		  set_cdr(next, new);
		}
		next = new;
		skip_comment_and_whitespace(rd);
		uint8_t c = io_peek_u8(rd);
		if(c == 0 || c == ')'){
		  io_read_u8(rd);
		  break;
		}
		if(c == '.'){
		  var save = *rd;
		  io_read_u8(rd);
		  var loc = io_getloc(rd);
		  skip_comment_and_whitespace(rd);
		  var nextloc = io_getloc(rd);
		  if(loc == nextloc){
			 *rd = save;
		  }else{
			 var v = tokenize_stream(rd);
			 set_cdr(next, v);
			 skip_comment_and_whitespace(rd);
			 if(io_read_u8(rd) != ')'){
				raise_string("Unexpected token");
			 }
			 break;
		  }

		}
	 }
	 return head;
	 
  }else{
	 skip_comment_and_whitespace(rd);
	 return read_token_data(rd);
  }
}


lisp_value lisp_read_stream(io_reader * rd){
  return tokenize_stream(rd);
}

lisp_value lisp_read_string(const char * str){

  io_reader w = io_reader_from_bytes((void *)str, strlen(str) + 1);
  w.offset = 0;
  return lisp_read_stream(&w);
}
