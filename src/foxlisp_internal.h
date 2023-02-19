// internals
//struct __binary_io;
void on_read_cons(io_reader * rd, lisp_value c);
lisp_value lisp_read_stream(io_reader * rd);

lisp_value cdr_nocheck(lisp_value v);

// modules
void load_lisp_base();
void load_hashtable_module();
void load_vector_module();
void foxgl_register();
void lisp_process_module_init();
void gc_register();

extern lisp_context * current_context;
