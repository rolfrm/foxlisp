(load "lisp1.lisp" )

(do-times 4 (lambda () (println (quote hej))))
(map! print (list 1 2 3 4 (quote x)))
(define x (macro (&rest b) b))

(println (x))

(println (read-string "(+ 1 2)"))
(println (eval (read-string "(+ 1 2)")))

(println (type-of (cons 1 2)))
(println (type-of "13"))

(print (- (+ 5 3 ) 1))
(println (if 1 (+ 2 3.5) 3))
(println (quote (1 2 3)))
(println (cons 1 2))
(println (+ 1 2))
(println (lambda (x) x))
(define y (+ 0.33333 5.0))
(println (+ y y))
((lambda (x y) (print (+ x y))) 5.0 50.9)
(define add (lambda (x y) (+ (println x) (println y))))
(println (add 123 321))

(when t (println "unless nil"))
(unless nil (println "__unless nil_-"))
(when nil (panic "This should not happen"))
(unless t (panic "This should not happen"))
(println '(1 2 3))
(println `(1 2 ,(+ 1 3)))


(assert (= 10 (funcall + 1 9)))

(define x 4)
(set! x 9)


(begin
 (println 1)
 (println 2)
 (println x))

(assert (cons? (cons 1 2 )))


(define libc (load-lib "libc.so.6"))
(define fopen (load-alien libc "fopen" native-null-pointer (list "pathname" "mode")))
(define fclose (load-alien libc "fclose" (integer 0) (list native-null-pointer)))

;size_t fread(void *restrict ptr, size_t size, size_t nitems,
;       FILE *restrict stream);
;
;size_t fwrite(const void *ptr, size_t size, size_t nitems,
;    FILE *stream);

(define fwrite (load-alien libc "fwrite" (integer 0) (list native-null-pointer (integer 0) (integer 0) native-null-pointer)))

(define fread (load-alien libc "fread" 0 (list native-null-pointer 0 0 native-null-pointer)))

(define malloc (load-alien libc "malloc" native-null-pointer (list (integer 0))))
(define free (load-alien libc "free" nil (list native-null-pointer)))

(println (list fopen fclose))

(let ((file (fopen "./test.x" "w+"))
		(data (make-vector 100 (byte 0)))
		)
  (vector-set! data 0 (byte 255))
  (vector-set! data 1 (byte 1))
  (vector-set! data 2 (byte 100))
  (println `(file:  ,file))
  (fwrite (vector-native-element-pointer data 0) 1024 1 file)
  (fclose file)
  )

(println (list '>> (lambda () 2)))

(let ((file (fopen "./test.x" "r"))
		(data (make-vector 10 (byte 0)))
		)
  
  (let ((read (lambda () (fread (vector-native-element-pointer data 0) 1 10 file)))
		  (r 0)
		  )
	 (loop (not (= 0 (set! r (read))))
		 (println r)
		 (println data)
		 )
	 )
  (fclose file)
  )

(let ((vector (make-vector 3 0)))
  (vector-set! vector 0 10)
  (vector-set! vector 1 555)
  (vector-set! vector 2 555)
  (println (list (vector-element-type vector) ': (vector-ref vector 0) (vector-ref vector 2) (vector-ref vector 1)))
  (println 'hello)
  (println (list 'vector 'pointer ': (vector-native-element-pointer vector 0)))
  (vector-resize vector 5)
  (println (list 'resized vector))
  )
(println (type-of 1))

(assert (and 1 1 2))
(assert-not (and 1 1 2 nil))
(assert-not (and nil))
(assert (and t))
(assert (and))

(assert (or 1 nil))
(assert-not (or nil nil))
(assert (or nil 1))


(println "Tests Passed")
