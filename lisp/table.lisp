(load "lisp1.lisp")

(defvar table-type (typespec-new 'table))

(defun list-to-native-vector (list)
    (let ((i 0)
        (v (make-native-vector (length list))))
    
    (while list
      (vector-set! v i (car list))
      (set! list (cdr list))
      (set! i (+ i 1)))
    v
    ))

(defun table-new (name columns)
    (let ((table (list name 
                       (list->vector (map columns car)) 
                       (list-to-native-vector (map columns (lambda (col) (make-native-vector 0 (cadr col))))))))
        (cons table-type (list-to-native-vector table))
        ))

(defun table-columns (table) (vector-ref (cdr table) 2))

(defun table-rows (table)
   (vector-length  (vector-ref (table-columns table) 0)))
    
(defun is-table (table) (eq (car table) table-type))

(typespec-set-print! table-type 
    (lambda (x) (cdr x)))

(defun table-push-row (table &rest args)
    (let ((rowcnt (table-rows table)))
    (for-each col (table-columns table)
        (vector-resize col (+ rowcnt 1))
        (when args
            (vector-set! col rowcnt (car args)))
        (set! args (cdr args))
        )
    ))
(defun table-insert-row (table index &rest args)
    (let ((rowcnt (table-rows table)))
    (for-each col (table-columns table)
        (vector-insert! col index (car args))
        (set! args (cdr args))
        )
    ))
(defun table-insert-row2 (table index args)
    (let ((rowcnt (table-rows table)))
        (for-each col (table-columns table)
            (vector-insert! col index (car args))
            (set! args (cdr args))
        )
    ))

(defun table-insert (table &rest args)
    (let ((index (table-insert-index table (car args))))
        (table-insert-row2 table index args)
        )
    )
(defvar table-id-iterator 1)
