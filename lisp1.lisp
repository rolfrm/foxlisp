(define defun (macro (name args &rest body) `(define ,name (lambda ,args ,@body))))

(define defmacro (macro (name args &rest body) `(define ,name  (macro ,args ,@body))))

(define list (lambda (&rest x) x))
(define begin progn)
(define set! set)
(define display println)

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
  `(when (not ,test) ,@body))

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

(defmacro assert (test text)
  `(unless ,test (panic (cons ,(if text text "assertion failed") ',test)))) 
