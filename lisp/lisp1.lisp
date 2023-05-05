
(def defun (macro (name args &rest body)
						`(def ,name (lambda ,args ,@body))))

(def defmacro (macro (name args &rest body)
                        `(def ,name  (macro ,args ,@body))))
(defmacro define (var value)
  `(def ,var ,value))

;(defmacro let (vars &rest body)
;  `(+let-impl+ ,vars ,@body))
(define let +let-impl+)

(define defvar define)

(def list (lambda (&rest x) x))

(def begin progn)
(def set! set)
(def display println)
(def eq =)
(def neq /=)
(def #f ())
(defun not (x) (= x nil))
(defmacro test! (x)
  (if lisp:*test-enabled*
      x
      nil))
(define append2 
    (lambda (lst)
      (if (not (cdr lst))
          (car lst) 
          (cons (car lst) (append2 (cdr lst))))))

(define append 
    (lambda (&rest lst) 
      (append2 lst)))

(defmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &rest body)
  `(if ,test () (progn ,@body)))

(defmacro unless!(test &rest body)
  (when (eval test) body))

(defun map (lst f2)
  (when lst
    (cons (f2 (car lst))
          (map (cdr lst) f2))))


(defun mapi2 (lst f2 i)
  (when lst
    (cons (f2 i (car lst))
          (mapi2 (cdr lst) f2 (+ i 1)))))

(defun mapi (lst f2)
  (mapi2 lst f2 0))

(defun map! (f2 lst)
  (loop lst
        (f2 (car lst))
        (set! lst (cdr lst)))
  nil)

(defun remove-first (elem lst)

  (if (eq elem (car lst))
      (cdr lst)
      (when lst 
        (cons (car lst) (remove-first elem (cdr lst)))
      )))

(defun find (elem lst)
  (if (eq elem (car lst))
      lst
      (when lst
        (find elem (cdr lst))
        )))


(defun do-times(n f2)
  (let ((x 0))
    (loop (< x n)
          (f2 x)
          (set x (+ 1 x))
          )))

(defun apply (f args)
  (eval (cons f args)))

(defun funcall (f &rest args)
  (apply f args))

(define nil? not)
(define null? not)
(define pair? cons?)

(defun integer? (v) (= (type-of v) 'INTEGER))
(defun rational? (v) (= (type-of v) 'RATIONAL))
(defun string? (v) (= (type-of v) 'STRING))
;(defun symbol?(p) (= (type-of p) 'SYMBOL)) 
(defun macro? (s) (eq (type-of s) 'FUNCTION_MACRO))
(defun procedure? (s) (eq (type-of s) 'FUNCTION))
(defun hashtable? (s) (eq (type-of s) 'HASHTABLE))

(defun unbound? (s) (not (bound? s)))
(define string=? string=)

(defun last (lst)
  (if (cdr lst) (last (cdr lst)) (car lst)))

(defun symbol-macro? (s) (and (bound? s t) (macro? (symbol-value s) 'FUNCTION_MACRO)))
(defun symbol-procedure? (s) (and (bound? s t) (procedure? (symbol-value s))))

(defmacro assert (test text)
  `(unless ,test (panic ,(or text "assertion failed"))))
(defmacro assert-not (test text)
  `(when ,test (panic (cons ,(if text text "assertion failed") ',test)))) 

(defmacro while (test &rest body)
  `(loop ,test ,@body))

(defmacro do (&rest body)
  `(progn ,@body))

(define eq? =)

(defun equals? (a b)
  (let ((type (type-of a)))
    (when (= type (type-of b))
      (if (= type 'STRING)
          (string= a b)
      (if (= type 'CONS)
          (and (equals? (car a) (car b))
               (equals? (cdr a) (cdr b)))
          (= a b))))))

(defun cdddr (l)
  (cdr (cddr l)))

(defun cddddr (l)
  (cddr (cddr l)))
(defun caaar (l)
  (car (caar l)))
(defun cadar (l)
  (car (cdar l)))

(defun caddr (l)
  (car (cddr l)))

(defun cadddr (l)
  (car (cdddr l)))

(defun caar (x)
  (car (car x)))
(defun cdar (x)
  (cdr (car x)))

(defmacro pop! (lst)
  `(let ((fst (car ,lst)))
     (set! ,lst (cdr ,lst))
    fst))
(defmacro swap (position value)
  `(let ((v ,position))
    (set! ,position ,value)
    v))

(defmacro push! (lst v)
  `(set! ,lst (cons ,v ,lst)))

(defun push-back! (lst v)
  (loop (cdr lst)
     (set! lst (cdr lst)))
  (set-cdr! lst (cons v nil)))

(defmacro match (var lookup &rest body)
  `(let ((,var ,lookup))
     (when ,var ,@body)))

(defun list-to-array (list map)
  (let ((i 0)
        (v (make-vector (length list) (float32 0))))
    
    (while list
      (vector-set! v i (float32 (car list)))
      (set! list (cdr list))
      (set! i (+ i 1)))
    v
    ))
(defun list->vector (list map)
  (let ((i 0)
        (v (make-vector (length list))))
    
    (while list
      (vector-set! v i (car list))
      (set! list (cdr list))
      (set! i (+ i 1)))
    v
    ))



(defmacro incf (var value)
  `(set! ,var (+ ,var ,value)))

(defmacro decf (var value)
  `(set! ,var (- ,var ,value)))

(defmacro dotimes! (a n &rest body)
  `(let ((,a 0))
    (loop (< ,a ,n)
          ,@body
          (incf ,a 1))
    nil
    ))


(defun memq (obj list)
  (let ((result nil))
    (while list
           (when (= (car list) obj)
             (set! result list)
             (set! list nil)
             )
           (set! list (cdr list)))
    result))

(defmacro let* (vars &rest body)
  (defun ilet(vars body)
    (if vars
        (let ((var (car vars)))
          `(let (,var)
             ,(ilet (cdr vars) body)))
        `(let () ,@body))
    )
  (ilet vars body))
(defun build-arg-list (args)
  (if (cons? args)
      (cons (car args) (build-arg-list (cdr args)))
      (if (null? args)
          nil
          (list '&rest args))))

(defmacro define (var &rest value)
  (if (cons? var)
      `(defun ,(car var) ,(build-arg-list (cdr var)) ,@value)
      `(def ,var ,(car value))))

(defun reverse! (lst)
  (when lst
    (let ((head lst))
      (set! lst (cdr lst))
      (set-cdr! head nil)
      (while lst
             (let ((new-head lst))
               (set! lst (cdr new-head))
               (when new-head
                 (set-cdr! new-head head)
                 )
               (set! head new-head)
               )
             )
      head)))

(defun take (f list)
  (let ((result nil))
    (loop list
          (when (f (car list))
            (set! result (cons (car list) result)))
          (set! list (cdr list)))
    (reverse! result)))
(test! (assert (eq 4 (length '(1 2 3 4)))))
(test! (println (take (lambda (x) (> x 10)) '(0 1 2 -2 3 -5 -9))))
(test! (assert (nil? (take (lambda (x) (> x 10)) '(0 1 2 -2 3 -5 -9)))))
(test! (assert (eq 3 (length (take (lambda (x) (> x 0)) '(0 1 2 -2 3 -5 -9))))))

(println lisp:*test-enabled*)
(defun first (f list)
  (let ((result nil))
    (loop list
          (when (f (car list))
            (set! result (car list))
            (set! list nil))
          (set! list (cdr list)))
    result))


(define hashtable-set! hashtable-set)

(define lisp:descriptions (make-hashtable))

(defun lisp:write-doc (item signature)
  (hashtable-set! lisp:descriptions item signature)
  )

(defun lisp:describe (item)
  ;(println (list 'describe item))
  (hashtable-ref lisp:descriptions item)
  )

(lisp:write-doc make-hashtable '(make-hashtable weak-keys weak-values))
(lisp:write-doc hashtable-set! '(hashtable-set! hashtable key value))
(lisp:write-doc hashtable-ref '(hashtable-ref hashtable key))
(lisp:write-doc defun '(defun name args &rest body))
(lisp:write-doc define '(define name value))
(lisp:write-doc vector-set! '(vector-set! vector index value))
(lisp:write-doc make-vector '(make-vector count default-value ))
(lisp:write-doc plookup '(plookup list sym))
(lisp:write-doc map! '(map! f list))
(defmacro def-wrap (name lib fname &rest params)
  (let ((argnum (if (integer? (car params))
                    (car params)
                    (length params)))
        (name2 (if (symbol? fname) (symbol->string fname) fname)))
    (println name)
    (let ((r `(def ,name (load-wrap ,lib ,name2 ,argnum))))
      (unless (integer? (car params))
        (set! r `(progn ,r
                  (lisp:write-doc ,name '(,name ,@params)))))
      r
      )))



(defun list*1(lists)
  (let ((head (car lists))
        (tail (cdr lists)))
    (if tail
        (cons head (list*1 tail))
        head)))

(defun list* (&rest lists)
  (list*1 lists))

(defmacro pop (place)
  `(let ((v (car ,place)))
    (set! ,place (cdr ,place))
    v))

(defun lisp:concat-2 (head lst lists)
  (cons head
        (if lst
            (lisp:concat-2 (car lst) (cdr lst) lists)
            (concat-1 lists))))

(defun concat-1 (lists)
  (let ((head (car lists))
        (tail (cdr lists)))
    (if tail
        (if head
            (lisp:concat-2 (car head) (cdr head) tail)
            (concat-1 (cdr lists)))
            
        head)))
        
(defun concat (&rest lists) (concat-1 lists) )

(defun concat-2 (a b)
  (if a
		(cons (car a) (concat-2 (cdr a) b))
		b))
										 
(defmacro for-each (arg list &rest body)
  `(let ((lst ,list)
         (,arg nil))
    (if (vector? lst)
      (let ((it 0) (cnt (vector-length lst)))
        (loop (< it cnt)
          (set! ,arg (vector-ref lst it))
          ,@body
          (incf it 1)
          )
        )
    (loop lst
         (set! ,arg (car lst))
         ,@body
         (set! lst (cdr lst))
         )  
      )
    ))

(define pi 3.141592653589793)
(define pi_2 (/ pi 2.0))

(defun lisp:function-location (func)
  (let ((fcode (function->code func)))
	(lisp:code-location fcode)))
