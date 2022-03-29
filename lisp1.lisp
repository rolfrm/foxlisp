
(def defun (macro (name args &rest body) `(def ,name (lambda ,args ,@body))))

(def defmacro (macro (name args &rest body)
                        `(def ,name  (macro ,args ,@body))))
(defmacro define (var value)
  `(def ,var ,value))

(def list (lambda (&rest x) x))
(def begin progn)
(def set! set)
(def display println)
(def eq =)
(def #f ())
(defun not (x) (= x nil))
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

(defun map! (f lst) 
  (loop lst 
       (f (car lst))
       (set lst (cdr lst))  
       ))

(defun map (lst f2)
  (when lst
    (cons (f2 (car lst))
          (map (cdr lst) f2))))

(defun do-times (n f2)
  (let ((x 0))
    (loop (< x n)
       (set x (+ 1 x))
       (f2 x))))

(defun apply (f args)
  (eval (cons f args)))

(defun funcall (f &rest args)
  (apply f args))

(defun nil? (v) (not v))
(defun null? (v) (not v))
(defun cons? (v) (= (type-of v) 'CONS))
(define pair? cons?)
(defun list? (x) (or (null? x) (cons? x)))

(defun integer? (v) (= (type-of v) 'INTEGER))
(defun rational? (v) (= (type-of v) 'RATIONAL))
(defun string? (v) (= (type-of v) 'STRING))
(defun symbol? (p) (= 'SYMBOL (type-of p))) 
(defun macro? (s) (eq (type-of s) 'FUNCTION_MACRO))
(defun procedure? (s) (eq (type-of s) 'FUNCTION))
(defun symbol-macro? (s) (macro? (symbol-value s) 'FUNCTION_MACRO))
(defun symbol-procedure? (s) (procedure? (symbol-value s)))
(defun unbound? (s) (not (bound? s)))
(define string=? string=)

(defun last (lst)
  (if (cdr lst) (last (cdr lst)) (car lst)))

(defun and (&rest items)
  (let ((it items)
        (ok t))
    (loop it
         (if (car it)
             (set! it (cdr it))
             (progn
               (set! ok nil)
               (set! it nil))))
    ok
    ))

(defun or (&rest items)
  (let ((it items)
        (ok nil))
    (loop it
         (let ((fst (car it)))
           (if fst
               (progn
                 (set! ok fst)
                 (set! it nil))
               (set! it (cdr it))
               
               )))
    ok
    ))


(defmacro assert (test text)
  `(unless ,test (panic (cons ,(if text text "assertion failed") ',test)))) 

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

(defun cddr (l)
  (cdr (cdr l)))

(defun cadr (l)
  (car (cdr l)))

(defun cdddr (l)
  (cdr (cddr l)))
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

(defmacro push! (lst v)
  `(set! ,lst (cons ,v ,lst)))

(defmacro match (var lookup &rest body)
  `(let ((,var ,lookup))
     (when ,var ,@body)))

(defun plookup (lst sym)
  (let ((r nil))
    (while lst
      (let ((fst (car lst)))
        (if (symbol? fst)
            (if (eq fst sym)
                (do
                 (set! r (cadr lst))
                 (set! lst nil))
                (set! lst (cddr lst)))
            (set! lst (cdr lst)))))
    r))

(defun list-to-array (list)
  (let ((i 0)
        (v (make-vector (length list) (float32 0.0))))
    (while list
      (vector-set! v i (float32 (car list)))
      (set! list (cdr list))
      (set! i (+ i 1)))
    v
    ))

(defmacro incf (var value)
  `(set! ,var (+ ,var ,value)))

(defmacro cond (&rest args)
  (defun condi (args)
    (println (caar args))
    (if args
        (if (eq (caar args) 'else)
            (cadar args)
            `(if ,(caar args)
                 ,(cadar args)
                 ,(condi (cdr args))))
        nil))
  (condi args))

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

(define hashtable-set! hashtable-set)

(define libc (load-lib "libc.so.6"))
(define fopen (load-alien libc "fopen" native-null-pointer (list "pathname" "mode")))
(define fclose (load-alien libc "fclose" (integer 0) (list native-null-pointer)))
(define fwrite (load-alien libc "fwrite" (integer 0) (list native-null-pointer (integer 0) (integer 0) native-null-pointer)))

(define fread (load-alien libc "fread" 0 (list native-null-pointer 0 0 native-null-pointer)))

(define malloc (load-alien libc "malloc" native-null-pointer (list (integer 0))))
(define free (load-alien libc "free" nil (list native-null-pointer)))
