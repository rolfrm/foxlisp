// internals
//struct __binary_io;
void on_read_cons(io_reader * rd, lisp_value c);
lisp_value lisp_read_stream(io_reader * rd);
void load_lisp_base();
