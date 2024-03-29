(load "lisp1.lisp")
(load "table.lisp")

(defun gen-id ()
    (incf table-id-iterator 1)
  )
(let ((tab1 (table-new 'entities '((entity 0 :entity)(x 0.0) (y 0.0))))
     (active-entities (table-new 'active-entities '((entity 0 :entity))))
     (hp-tab (table-new 'hp '((entity 0 :entity) (hp 0))))
     (hp-lookup (table-new 'hp-lookup '((entity 0 :entity) (hp 0))))
     (models (table-new 'models '((entity 0 :entity) (model ()))))
    )
    (let ((a (gen-id))
         (b (gen-id))
        (c (gen-id))
        (d (gen-id)))
    
    (table-push-row tab1 a 2.3 4.5)
    (table-push-row tab1 b 3.1 2.2)
    (table-insert tab1 c 4.0 4.0)
    (table-insert tab1 d 4.0 4.0)
    (table-insert tab1 (gen-id) 4.0 4.0)
    (table-insert tab1 (gen-id) 4.0 4.0)
    (table-insert tab1 (gen-id) 3.0 3.0)
    (table-insert active-entities a)
    (table-insert active-entities b)
    (table-insert active-entities c)
    (table-insert models a 'mesh_123)
    (table-insert models b 'mesh_5)
    ;(table-insert models c 'mesh_123)
    (println models)
    (table-insert hp-tab (gen-id) 66)
    (table-insert hp-tab (gen-id) 66)
    (table-insert hp-tab a 10)
    (table-insert hp-tab b -5)
    (table-insert hp-tab d -10)
    (println "generating rows")
    (dotimes! i 10
        (table-push-row tab1 (gen-id) (+ 1.0 i) (- 2.13 i))
        
        )
    (println "updating rows")
    (table:iter tab1
        (set! x (+ x 0.1))   
    )

	  
        
    (println "done updating rows")
    
    (table:select active-entities hp-tab hp-lookup (> hp -10))
    (table:iter hp-lookup :edit 
        (println (list entity hp))
        (set! hp (- hp 1))
		  (when (> hp 0)
			 (println 'remove)
			 :remove
			 )
        )

		(table:iter hp-lookup  
        (println (list '--->> entity hp ))

        )
		(println 'fancy-iter)
		(table:iter (active-entities hp-tab models)
						(println (list 'found hp entity model))
						)
		(println ',,,)
		;(println active-entities)
		(table:iter (active-entities (:optional hp-tab) (:optional models))
						(println (list 'found hp entity " " model))
						)
    
    ;(println tab1 " " hp-tab " " active-entities " " hp-lookup)
    (table:clear hp-lookup)
    ;(println tab1 " " hp-tab " " active-entities " " hp-lookup)
    ))

