(load "lisp1.lisp")

(defvar table-type (typespec-new 'table))

(defun table-new (name columns)
    (let ((table (list name (map columns cdr) (map columns (lambda (col) (make-vector 1 (cadr col)))))))
        (cons table-type (list->vector table))
        ))

(defun is-table (table) (eq (car table) table-type))

(typespec-set-print! table-type 
    (lambda (x) (cdr x)))

(println (table-new 'entities '((entity 0)(x 0.0) (y 0.0))))
(println "HELLO")
(defun gen-table()
    nil)



'(deftable table1 
    (column entity :int)
    (column x :float32)
    (column y :float32)
    )
    

'(let ((t1 (table1-new)))
    (insert t1 1 1.0 2.0)
    (insert t1 2 1.4 1.0)
    (println t1)
    )