
(load "lisp1.lisp")

(println (+ 1 2))

(defun test ()
  (println (+ 4 5 )))

(defvar ht (make-hashtable))
(println ht)
(test)

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
     (when (and (integer? a) (integer? b))
	 (emit WASM-I64-CONST :leb a)
	 (emit WASM-I64-CONST :leb b)
	 (emit WASM-I64-ADD)
	 'I64
	 )
    )
  )
(defun op-lisp-value-+ (args)
  (let ((a (car args))
        (b (cadr args))
		  )
	 (println args 'yes!)
	 'lisp-value
	 
    ))


(println byte-buffer-new)
(defvar il (byte-buffer-new))
(defun emit (&rest args)
  (eval (concat (list 'byte-buffer-write 'il) args) (lisp:get-current-scope!!)))

(op-i64-+ 1 2)
(defvar funcmap (make-hashtable))

(defvar functions (make-hashtable))
(defvar locals nil)

(defun defgeneric (name func type)
  (let ((c (cons (list func type) (hashtable-ref funcmap name))))
	 (hashtable-set! funcmap name c)
  ))


(defun compile-defun(code)
  (let ((name (car code))
		  (args (cadr code))
		  (body (cddr code)))
	 (for-each arg args
				  (push! locals (cons arg 'lisp-value)) 
				  )
	 
	 (println (list '>> name args body locals))
	 (loop body
			(let ((statement (car body)))
			  (println 'sub: statement (compile-code statement :any))
			  )
			(set! body (cdr body))

			)
	  (for-each arg args
					(pop locals)
					)
  ))

(defgeneric '+ op-i64-+ '(i64 i64 i64))
(defgeneric '+ op-f64-+ '(f64 f64 f64))
(defgeneric '+ op-lisp-value-+ '(lisp-value lisp-value lisp-value))
(defgeneric 'defun compile-defun :macro)


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
					(set! funcs (cdr funcs)))))))
									 
					  

	 ((integer? code) (emit WASM-I64-CONST :leb code) 'i64) 
	 ((rational? code) (emit WASM-F64-CONST  code) 'f64)
	 ((symbol? code)
	  (let ((l (clist-get locals code)))
		 (if l
			  (cdr l)

			  (panic (list code 'symbol-not-implemented)))))

	 ))
	

(compile-code '(defun add2(x y) (+ x y)))
(println 'finished)
(when nil
(compile-code '(defun add2(x y) (+ x y)))

  (compile-code '(let ((x 0 int))
						
						(set! x 3)
						(print x))))

