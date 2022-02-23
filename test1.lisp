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
