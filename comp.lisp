
(load "lisp1.lisp")

"types
  
  numbers: i64, f64, i32, f32, u8, fixnum, symbols
  reference counted: cons, vector, string, bignum
  aggregated types: (cons T) (cons T N), (vector T N), (vector T)
  lisp-value: the union of all other types with a type tag.
  e.g
   - (cons i64): car can only be i64, cdr can only be (cons i64)
   - (cons i64 10): a list of i64 of 10 elements. (cdr) yields a (cons i64 9).
   - (vector i64 10): a vector of 10 i64s.
   - (vector i64): a vector of i64s. 
  function types: (funcion (ARG) RET)
    e.g
    - (function (i64 i64) i64)
    - (function (&rest (cons i64)) i64)
    - (function (lisp-value) lisp-value)

the stack.
Lets assume 8 byte alignment.
Any value <= 8 byte are pushed trivialy.
lisp value (16 bytes) takes up two slots on the stack. One for the type tag (8 bytes) and one for the value(8 bytes).


"

(defvar WASM-I64-CONST 0x41)
(defvar WASM-I64-ADD 0x7C)
(defvar WASM-F64-CONST 0x44)
(defvar WASM-F64-ADD 0xA0)

(defun op-f64-+ (args)
  (let ((a (car args))
        (b (cadr args))
		  )
	 (when (and (eq a 'f64) (eq b 'f64))
		(emit WASM-F64-ADD)
    )
  ))

(defun op-i64-+ (args)
  (let ((a (car args))
        (b (cadr args))
    )
    (when (and (eq a 'i64) (eq b 'i64))
		(emit 'I64-ADD)
		'I64
		)
    )
  )

(defun op-lisp-value-+ (args)
  (let ((a (car args))
        (b (cadr args))
		  )
	 (emit 'call 'lisp-value-+)
	 'lisp-value
    ))


(println byte-buffer-new)
(defvar il (list))
(defun emit (&rest args)
  (set! il (cons args il)))

(op-i64-+ 1 2)
(defvar funcmap (make-hashtable))

(defvar functions (make-hashtable))
(defvar locals nil)
(defvar globals (make-hashtable))
(defun defgeneric (name func type)
  (let ((c (cons (list func type) (hashtable-ref funcmap name))))
	 (hashtable-set! funcmap name c)
  ))


(defun compile-defun(code)
  
  (let ((name (car code))
		  (args (cadr code))
		  (body (cddr code))
		  (prev-il il))
	 (set! il nil)
	 (for-each arg args (push! locals (list arg 'lisp-value (length locals))))
	 (loop body
			(let ((statement (car body)))
			  (println 'sub: statement (compile-code statement :any))
			  )
			(set! body (cdr body))

			)
	 ;(for-each arg args
													 ;			  (pop! locals)
	 (let ((il-code (reverse! il)))
		(println (list 'func: name args locals il-code));			  )
		(hashtable-set! globals name (list 'function name args locals il-code))
		(set! locals nil)
		(set! il prev-il)
	 )))

(defun compile-let (code)
  (let ((args (car code))
		  (body (cdr code)))
	 (for-each arg args
				  (compile-code (cadr arg) 'lisp-value)
				  (emit 'set-local (length locals))
				  (push! locals (list (car arg) 'lisp-value (length locals)))

				  )
	 (for-each elem body
				  (compile-code elem :any))
	 ))

(defgeneric '+ op-i64-+ '(i64 i64 i64))
(defgeneric '+ op-f64-+ '(f64 f64 f64))
(defgeneric '+ op-lisp-value-+ '(lisp-value lisp-value lisp-value))
(defgeneric 'defun compile-defun :macro)
(defgeneric 'let compile-let :macro)


(defun compile-code (code type)
  (cond
	 ((cons? code)
	  (let ((head (car code))
			  (args (cdr code)))
		 (let ((argcnt (length args)))
        (unless (symbol? head)
          (error "first arg must be a symbol")
          )
        (let ((funcs (hashtable-ref funcmap head)))
			 (if funcs
				  (loop funcs
						 (let ((types (cadr (car funcs))))
							(let ((match-types
									 (if (eq types :macro)
										  (if (eq argcnt (- (length types) 1))
												t
												nil))))
							  (if (eq types :macro)
							  ((caar funcs) args)
							  (let ((args2 ())
									  (type-it types)
									  (arg-it args)
									  )
								 (loop (and arg-it type-it)
									 (let ((t2 (compile-code (car arg-it) (car type-it))))
										(push! args2 t2)

										)
									 (set! arg-it (cdr arg-it))
									 (set! type-it (cdr type-it))

									 )
								 (println 'args2: args2 '- (car funcs))
								 (let ((r ((caar funcs) args2)))
									(println r)
									(when 
										 (progn
											(set! funcs nil)
											r)))
										 
										 

								 ))))
						 (set! funcs (cdr funcs)))
				  (let ((glob (hashtable-ref globals head)))
					 (when glob
						(for-each arg args
									 (let ((type (compile-code arg 'lisp-value)))
										
										))
						  (emit 'call head))
						  
					 ))
				  

				  ))))
									 

	 ((integer? code) (emit 'load-const code) 'i64) 
	 ((rational? code) (emit 'load-const  code) 'f64)
	 ((symbol? code)
	  (let ((l (clist-get locals code)))
		 (if l
			  (progn
				 (emit 'load-local (caddr l))
				 (cdr l))

			  (panic (list locals code 'symbol-not-implemented)))))

	 ))

 	

(compile-code '(defun add2(x y)
					 (let ((z 0))
						(let ((a 1)
								(b 2))

						  )
						(+ (+ x y) z))))

(compile-code '(defun add3(w)
					 (add2 w w)))

(defun il:get-functions()
  (let ((l nil))
	 (hashtable-iter globals
						  (when (eq (car value) 'function)
							 (push! l value)

						  ))
	 l))

(defun build-type-section()
  (let ((types nil))
	 (for-each f (il:get-functions)
				  (push! types (cons #x60 (cons (list :leb (length (caddr f)))
														  nil
														  

														  ))) 
				  )
	 (println 'type-section (reverse! types))
	 
	 ))
  


(defun build-wasm-module()
  (build-type-section)
  )
(println (il:get-functions))

(build-wasm-module)

(defun soft-eval (code)
  (if (cons? code)
		(let ((fun (hashtable-ref globals (car code))))
		  
		  (let ((locals (
				  (stack (map (cdr code) soft-eval)))
			 (println fun)
			 (let ((fun-code (cddddr fun)))
				(for-each elem (car fun-code)
							 
							 (println elem)))))
		code
  ))
(cddddr (list 1 2 3 4 5 6 7))
(soft-eval '(add3 1))
