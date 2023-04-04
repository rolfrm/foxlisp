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
        (println index " " (car args))
        (table-insert-row2 table index args)
        )
    )

(let ((tab1 (table-new 'entities '((entity 0)(x 0.0) (y 0.0))))
     (active-entities (table-new 'active-entities '((entity 0))'))
     (hp-tab (table-new 'hp '((entity 0) (hp 0))))
     (hp-lookup (table-new 'hp-lookup '((entity 0) (hp 0))))
    )
    (table-push-row tab1 1 2.3 4.5)
    (table-push-row tab1 2 3.1 2.2)
    (table-insert tab1 4 4.0 4.0)
    (table-insert tab1 10 4.0 4.0)
    (table-insert tab1 3 4.0 4.0)
    (table-insert tab1 -3 4.0 4.0)
    (table-insert tab1 3 3.0 3.0)
    (table-insert active-entities 4)
    (table-insert active-entities 3)
    (table-insert active-entities 2)
    (table-push-row hp-tab 2 10)
    (table-push-row hp-tab 3 -5)
    (table-push-row hp-tab 10 -10)
    
    (table:select active-entities hp-tab hp-lookup (< hp 100))
    (table:iter hp-lookup 
        (println (cons hp entity))
        (set! hp (- hp 1))
        )
    
    (println tab1 " " hp-tab " " active-entities " " hp-lookup)
    (table:clear hp-lookup)
    (println tab1 " " hp-tab " " active-entities " " hp-lookup)
    )
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
    