


(load "lisp1.lisp" )


(let ((x2 10))
  (println x2))
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

(assert (= 15 (+ 1 2 3 4 5)))
(assert (= 8 (* 2 2 2)))
(assert (= 8.0 (* 2 2 2.0)))

(assert (= 10 (funcall + 1 9)))
(assert (eq 1 '1)) ; '1 is the number 1.
(define x 4)
(set! x 9)

(begin
 (println 1)
 (println 2)
 (println x))

(assert (cons? (cons 1 2 )))

(let ((vector (make-vector 3 0)))
  (vector-set! vector 0 10)
  (vector-set! vector 1 555)
  (vector-set! vector 2 555)
  (println (list (vector-element-type vector) ': (vector-ref vector 0) (vector-ref vector 2) (vector-ref vector 1)))
  (println 'hello)
  (println (list 'vector 'pointer ': (vector-native-element-pointer vector 0)))
  (set! vector (vector-resize vector 5))
  (println (list 'resized vector (vector-length vector)))
  (assert (eq (vector-length vector) 5))
  )
(println (type-of 1))


(assert (println (and 1 1 2)))
(assert-not (println (and 1 1 2 nil)))
(assert-not (and nil))
(assert (and t))
(assert (and))

(assert (or 1 nil))
(assert-not (or nil nil))
(assert (or nil 1))

(assert-not (eq '(1 2 ( 3 4)) '(1 2 ( 3 4))))

(assert (equals? '(1 2 ( 3 4)) '(1 2 ( 3 4))))

(define x (cons nil nil))
(set-cdr! x (cons 2))
(set-car! x 1)

(assert (eq 0.2 .2))

(println (= 1 (car '(1 . 2))))

;(assert (equals? x '(1 2)))

(assert (equals? '(1 . 2) '( 1 . 2)))
(assert-not (equals? '(1 . 2) '(2 . 1)))
(assert (= 10 (do 1 2 10)))

(println '(:a))

(assert (= 3 (plookup '(:a 1 :b 2 :c 3 :d 4) :c)))
(assert-not (plookup '(:a 1 :b 2 :c 3 :d 4) :g))

(assert (symbol? 'test-sym))
(assert-not (symbol? 1))

(let ((v1 (make-vector 1 (float32 0))))
  (assert (= (vector-ref v1 0) (float32 0)))
  (vector-set! v1 0 (float32 1.0))
  (assert-not (= (float32 1.0) (float32 0.0)))
  (assert (= (float32 1.0) (float32 1.0)))
  (assert (= (vector-ref v1 0) (float32 1.0)))
  )

(let ((l '(3 4 5)))
  
  (assert (eq (println (pop! l)) 3))
  (assert (eq (println (pop! l)) 4))
  (push! l 6)
  (push! l 7)
  (assert (eq (println (pop! l)) 7))
  (assert (eq (pop! l) 6))
  
  )

(let ((ok nil))
  (match x (plookup '(:a 1 :b 2) ':b)
			(set! ok (eq x 2)))
  (assert ok))
(match x (plookup '(:a 1 :b 2) ':c)
		 (assert nil))

(let ((ht (make-hashtable)))
  (hashtable-set ht 5 10)
  (println (list ht (hashtable-ref ht 5)))
  (assert (eq 10 (hashtable-ref ht 5)))
  (assert-not (hashtable-ref ht 10))
  (hashtable-remove ht 5)
  (assert-not (hashtable-ref ht 5))
  )
(println (string->vector "123"))
(assert (equals? "123" (vector->string (string->vector "123"))))
(assert-not (= "123" (vector->string (string->vector "123"))))
(let ((expect '(1 2 3)))
  (assert (equals? expect (read-string (vector->string (string->vector (value->string expect))))))
  (assert-not (eq expect (read-string (vector->string (string->vector (value->string expect))))))
  )


(println (parse-hex "00cc"))
(println (hex-string 100 6))
(println (hex-string 1000000))
(assert (symbol? :a))
(assert (eq ':a :a))
(assert (eq :a :a))
(assert-not (eq '':a :a))

(println (list ':a :a '':a))

(define teststr "\"1\"" )

(define var1 5)
((lambda ()
  (define var1 6)
  (assert (eq var1 6))))
(assert (eq var1 5))

(let ((teststr "(:emacs-rex (swank:connection-info) ""COMMON-LISP-USER"" t 1)")
      (first :emacs-rex))
  (assert (eq (car (read-string teststr)) :emacs-rex)))

(defun test-cond (x)
  (cond
    ((symbol? x) 1)
    ((integer? x) 2)
    (else 3)))

(assert (eq 1 (test-cond 'a)))
(assert (eq 2 (test-cond 3)))
(assert (eq 3 (test-cond 3.0)))
(assert (eq 3 (cadr (println (memq :b (list :a 2 :b 3 :c 4))))))

(let* ((a 1) (b (+ a 1)) (c (+ b 1)))
  (assert (eq 3 c))
  (assert (eq 2 b))
  (assert (eq 1 a)))

(defun ht-stress ()
  (let ((ht (make-hashtable)))
    (do-times 20 (lambda (i)
                    (hashtable-set ht (cons i i) i)
                    (vector->string (string->vector (value->string '(1 2 3))))
                    ))))
(do-times 20 (lambda (i) (ht-stress)))

(println (eval '(progn (+ 1 2))))
;(println ("\\\"asd\\\""))
(define asdstr (read-string (read-string "\"asd\"")))
(define hej 10)
(println (symbol-procedure? 'apply))
(println asdstr)
(assert (unbound? 'hejjj))
(assert (bound? 'hej))

(define countup nil)
(define clear nil)
(let ((counter 0) (decrement ()))
  (set! countup (lambda ()
                  (set! counter (+ counter 1))
                  (push! decrement (lambda () (set! counter (- counter 1))))
                  counter
                  
                  ))
  (set! clear (lambda ()
                (map! funcall decrement)
                (set! decrement nil)
                nil
                ))
  )
(do-times 10 countup)
(println (countup))
(clear)
(println (countup))

(define asdasddsa222 (list 1 2 3))
(define asdasddsa221 (list 1 2 3))
(define asdasddsa223 '( 1 2 3))
(define asdasddsa224 '( 1 2 3))

(define (test-def x) (+ x 10))
(assert (eq (test-def 5) 15))
(define (test-def2 . x) (cons 10 x))
(assert (equals? (test-def2 5) '(10 5)))
   
(assert (eq (- 0 5) -5))

(println (lisp:all-symbols))
(assert (string-starts-with "asd" "as"))
(assert-not (string-starts-with "dsa" "as"))
(println (string-starts-with (symbol->string 'list) "li"))

(assert (equals? '(6 5 4 3 2 1) (reverse! '(1 2 3 4 5 6))))

(println (take (lambda (x) (string-starts-with (symbol->string x) "lis")) (lisp:all-symbols)))
(assert (equals? '(1 2 3) (take (lambda (x) (> x 0)) '(-1 1 -2 2 -3 3))))

(define exception-handled nil)
(define normal-form-completed nil)

(with-exception-handler
    (progn
      (panic "empty list")
      (set! normal-form-completed 10)
      )
  (lambda (ex)
    (set! exception-handled 1)
    (println "its ok")))
(assert exception-handled)
(assert-not normal-form-completed)

(let ((x 5))
  (println 'hej))
(println (list 'pre-gc (lisp:count-allocated)))
(lisp:collect-garbage)
(lisp:collect-garbage)
(list 1 2 3)
(println (list 'post-gc (lisp:count-allocated)))

(make-vector 100 0.0)
(println (list 'pre-gc (lisp:count-allocated)))
(lisp:collect-garbage)
(list 1 2 3)
(println (list 'post-gc (lisp:count-allocated)))


(for-each arg '(1 2 3 4) 
      (print arg))
(lisp:collect-garbage)

(do-times 10 (lambda ()
(let ((l '(2.0 1.0))
      (l2 0.0)
      (actions nil))
        
  (do-times 0
    (lambda ()
      (lisp:collect-garbage)
      (for-each f actions (f))
      (let ((f2 (lambda ()
                  (let ((v 0.0))
                    (map! (lambda (i) (incf v i)) l)
                    (push! l v)
                    (incf l2 v)
                    ))))
        (push! actions f2)
        (println l2)
        )))
  ;(println l)
  )))

(define test1:f nil)
(define test1:f2 nil)

(let ((a 1)
      (b 2))
  (set! test1:f
        (lambda ()
          (incf a 1)
          (incf b b)
          (+ a b)))
  (set! test1:f2
        (lambda ()
          (incf a 1)
          (incf b b)
          (+ a b)))

  )
(lisp:collect-garbage)
(lisp:collect-garbage)
(assert (eq 6 (test1:f)))
(println (test1:f))
(println (test1:f))
(println (test1:f2))

(loop nil
     (lisp:collect-garbage)

     (
      (let ((a 10))

        (let ((lmb (lambda ()
                     (println (cons (test1:f2) (test1:f))))))
          lmb
          ))))

(load "test3.lisp")
(defun try-print-color()
  (lisp:collect-garbage)
  (test-other))
(try-print-color)
(try-print-color)

(case 1
  (2 (println '2))
  (1 (println 'oook)))


(defmacro test-lambda-macro(x y)
  `(list ,x ,y))
(let ((c 2))
  (println ((lambda (a) (test-lambda-macro a c)) 1)))

(assert (eq 3 (eval '(+ 1 2))))
;; test eval with dynamic scope

(let ((x 10))
  (assert (eq 30 (eval '(* 3 x) (lisp:get-current-scope)))))

(let ((y 10))
  (assert (eq 40 (eval '(+ y x) (lisp:sub-scope (lisp:get-current-scope) 'x 30)))))

(let ((x 10))
  (lisp:scope-set! (lisp:get-current-scope) 'x 20)
  (assert (eq 20 x)))


(assert (eq 3.0 (+ 1.0 2)))

(assert (eq (/ 10 3.0 2.0) (/ 10.0 6.0)))

(assert (not (< 1 2 1)))
(assert (< 0 1 2 3))
(assert (= 1 1 1 1 1 1 1 1 1 1 1 1))
(assert (not (= 1 1 1 1 1 0 1 1)))
(assert (not (/= 1 2 3 4 5 1 6 7)))
(assert (/= 1 2 3 4 5 6 7 8))
(assert (/= 1 2))
(assert (not (/= 1 1)))
(assert (> 3 2 1 0))
(assert (not (> 1 2 3)))

(assert (eq 3 (min 6 9 102 9 3)))
(assert (eq 102 (max 6 9 102 9 3)))


(assert (or nil nil 2))
(assert (and t t t))
(assert-not (and t nil t))
(assert-not (or nil nil nil))
(println pi)

(assert (eq 4 (case :b
                (:a 2)
                (:c 3)
                (otherwise 4)
                (:d 5)
                (otherwise 6))))

(assert (eq 5 (println (case :d
                (:a 2)
                (:c 3)
                (:d 5)
                (otherwise 6)))))
(assert (eq 2 (case :d
                ((:a :b) 1)
                ((:c :d) 2)
                ((:e :f :g) 3)
                (otherwise 4))))

(assert (eq 2 (case 'otherwise
                (:a 1)
                ((otherwise) 2)
                (otherwise 3))))

(lisp:with-scope-binding (lisp:get-current-scope) nil nil '((x 10) (y (+ x 5)))
                         '((println (list x y))))

(let ((test-scope
       (let ((a 10) (b 20))
         (lisp:get-current-scope)))
      (test-scope2
       (let ((c 20) (d 30) (e 40))
         (lisp:get-current-scope))))
  (let ((r (lisp:with-scope-variable test-scope test-scope2 '(c d) 'x
                                     '((+ a b c d)))))
    (assert (eq r 80))
    )
  (let (( r (lisp:with-sub-scope test-scope 'c 30 '((+ a b c)))))
    (assert (eq r 60))
    ))
(println (remove-first 2 (list 1 2 3 4)))

(defvar asd '(asd))
;(println (list (hashtable-ref lisp:++cons-file-offset++ asd) (hashtable-ref lisp:++cons-file++ asd)))
;(println lisp:++cons-file++) 
;(println (cons 'lisp-current-file lisp:++current-file-ptr++))
;(println (lisp:code-location asd))

(defun test-func-1 () (+ 1 2))
;;; blank line!!
(defun test-func-2 () (+ 1 2))
;(println (car (function->code test-func-1)))
										;(println (hashtable-keys lisp:++cons-file-offset++))
;(assert (eq (cadr (println (lisp:function-location test-func-1)))
;            (- (cadr (println (lisp:function-location test-func-2))) 2)))

;(println (lisp:code-location (function->code test-func-1)))
                                        ;(println (hashtable-values lisp:++cons-file++))
;(println (length (hashtable-keys lisp:++cons-file-offset++)))

(dotimes! i 50
          (println i)
    (let ((a 
            (make-vector (+ 6000 (* i 100)) (byte 0))))
      
      ;(print (cons 'ok i))
      )
    (lisp:collect-garbage)
    )


(let ((a 10))
  (load "test1-lib.lisp")
  (println (test1-lib-test))
  (lisp:collect-garbage)
  (println (test1-lib-test))
  (print (deref-pointer (ref!! a)))
  )
(println (test1-lib-test))

(defvar scope1 (let ((rec2 1234)
                     (asd 123))
                 (set! rec2 (cons 1 2))
                 (lisp:get-current-scope)))
(println scope1)
(println (symbol-value 'asd scope1))




(let ((a1 '(1 2 3))
      (a2 '(1 2 3))
      (ht (make-hashtable :deep-equality))
      )
  (dotimes! i 5
            (dotimes! j 5
                      (hashtable-set ht (list i j 0) (* i j ))))
  (dotimes! i 5
            (dotimes! j 5
                      (hashtable-set ht (list i j 0) (* i j))))
  (hashtable-set ht a1 1)
  (println (hashtable-ref ht a2))
  (assert (eq (hashtable-ref ht '(3 4 0)) (* 3 4)))
  )

(defvar ht0 (make-hashtable :deep-equality))
(hashtable-set ht0 '(1 2 3) '(4 5 6))

(let ((x 1))
  (load "test4.lisp"))
(lisp:collect-garbage)

(defun test1 ()

  (dotimes! i 10000
			(cons 1 2)))
(defun test2()
  (dotimes! i 2
			(lisp:collect-garbage)
			(test1)))

(test2)


(println (test-4-a))

;(let ((a 3)
;	  (b 4))
  
;  (lisp:with-scope-binding (lisp:get-current-scope)


(defvar keystable (make-hashtable :keys 2))
(hashtable-setn! keystable 1 2 3)
(hashtable-setn! keystable 2 2 4)
(println (hashtable-refn keystable 1 2))
(println (hashtable-refn keystable 2 2))
(println (hashtable-values keystable))
;(println (hashtable-count keystable))
;(println (hashtable-values keystable))
;(println lisp:executable-path)

(let ((test-type (typespec-new 'test))
      (new-called nil)
      (destruct-called nil)
      (print-called nil)
  )
(typespec-set-construct! test-type (lambda () (set! new-called t)
  (cons test-type '1234)))
(typespec-set-destruct! test-type (lambda (x) (set! destruct-called t)))
(typespec-set-print! test-type (lambda (x) (assert (eq (cdr x) 1234)) (set! print-called t) (cdr x)))
(let ((item (typespec-instance test-type)))
  (println item)
)
  (lisp:collect-garbage)
  (assert new-called)
  (assert destruct-called)
  (assert print-called)
  (println (list new-called destruct-called print-called))
)
(let ((nvec (make-native-vector 100 0)))
  (vector-set! nvec 0 10)
  (vector-set! nvec 10 10)
  (println (vector-resize nvec 30))
  )
(defun test-varacic-perf (&rest args))
;; just verify that this does not cause us to run out of memory. (max cell limit.)
(assert (number? 1.0))
(assert (number? 1))
(defvar math-random-min 500.0)
(defvar math-random-max -500.0)
(dotimes! n 1000
			 (let ((r (math:random -5 1 )))
				(set! math-random-min (min math-random-min r))
				(set! math-random-max (max math-random-max r))))
(println math-random-min)
(println math-random-max)
(assert (eq -5 math-random-min))
(assert (eq 1 math-random-max))
;(println 'ok: math-random-min '--  math-random-max)
;(println (math:random 1 4))
													 ;(dotimes! n 1000000 (test-varacic-perf 10 10 10))

(println "Tests Passed")
