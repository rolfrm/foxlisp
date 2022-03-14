(define defun (macro (name args &rest body) `(define ,name (lambda ,args ,@body))))

(define defmacro (macro (name args &rest body) `(define ,name  (macro ,args ,@body))))

(define list (lambda (&rest x) x))
(define begin progn)
(define set! set)
(define display println)
(define eq =)

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

(defun do-times (n f2)
  (let ((x 0))
	 (loop (< x n)
		 (set x (+ 1 x))
		 (f2))))
  
(defun apply (f args)
  (eval (cons f args)))

(defun funcall (f &rest args)
  (apply f args))

(defun nil? (v) (not v))
(defun cons? (v) (= (type-of v) 'CONS))
(defun integer? (v) (= (type-of v) 'INTEGER))
(defun rational? (v) (= (type-of v) 'RATIONAL))
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

(defun equals? (a b)
  (let ((type (type-of a)))
	 (when (= type (type-of b))
		(if (= type 'CONS)
			 (and (equals? (car a) (car b))
					(equals? (cdr a) (cdr b)))
			 (= a b)))))

(defun cddr (l)
  (cdr (cdr l)))

(defun cadr (l)
  (car (cdr l)))

(defun cdddr (l)
  (cdr (cddr l)))

(defun caddr (l)
  (car (cddr l)))


(defmacro pop! (lst)
  `(let ((fst (car ,lst)))
	  (set! ,lst (cdr ,lst))
	  fst))

(defmacro push! (lst v)
  `(set! ,lst (cons ,v ,lst)))

(defmacro match (var lookup &rest body)
  `(let ((,var ,lookup))
	  (when ,var ,@body)))
		 

(defun symbol? (p) (= 'SYMBOL (type-of p))) 

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


(define libc (load-lib "libc.so.6"))
(define fopen (load-alien libc "fopen" native-null-pointer (list "pathname" "mode")))
(define fclose (load-alien libc "fclose" (integer 0) (list native-null-pointer)))
(define fwrite (load-alien libc "fwrite" (integer 0) (list native-null-pointer (integer 0) (integer 0) native-null-pointer)))

(define fread (load-alien libc "fread" 0 (list native-null-pointer 0 0 native-null-pointer)))

(define malloc (load-alien libc "malloc" native-null-pointer (list (integer 0))))
(define free (load-alien libc "free" nil (list native-null-pointer)))
