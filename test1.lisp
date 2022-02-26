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
(define y (+ 0.33333 5))
(println (+ y y))
((lambda (x y) (print (+ x y))) 5 50)
(define add (lambda (x y) (+ x y)))
(println (add 123 321))


(when t (println "unless nil"))
(unless nil (println "__unless nil_-"))
(when nil (panic "This should not happen"))
(unless t (panic "This should not happen"))
(println '(1 2 3))
(println `(1 2 ,(+ 1 3)))


(println (funcall + 1 9))

(define x 4)
(set! x 9)


(begin
 (println 1)
 (println 2)
 (println x))

(assert (cons? (cons 1 2 )))

(define df '(color
  :rgb (1 0 1)
				 (sphere :center (1 1 1) :radius 1.0)))
(println df)

(define libc (load-lib "libc.so.6"))
(define fopen (load-alien libc "fopen" native-null-pointer (list "pathname" "mode")))
(define fclose (load-alien libc "fclose" (integer 0) (list native-null-pointer)))

;size_t fread(void *restrict ptr, size_t size, size_t nitems,
;       FILE *restrict stream);
;
;size_t fwrite(const void *ptr, size_t size, size_t nitems,
;    FILE *stream);

(define fwrite (load-alien libc "fwrite" (integer 0) (list native-null-pointer (integer 0) (integer 0) native-null-pointer)))

(define malloc (load-alien libc "malloc" native-null-pointer (list (integer 0))))
(define free (load-alien libc "free" nil (list native-null-pointer)))

(println (list fopen fclose))

(let ((file (fopen "./test.x" "w+"))
		(data (malloc (integer 1024)))
		)
  (println `(file:  ,file))
  (fwrite data (integer 1024) (integer 1) file)
  (fclose file)
  (free data)
  )
(let ((vector (make-vector (integer 3) 0)))
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

