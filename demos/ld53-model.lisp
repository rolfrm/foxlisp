(load "models-2.lisp")
(load "models-3.lisp")
(load "table.lisp")
(println 'load-demo-4)
(defvar ui:entity-id nil)
(defvar ui:next-id 0)
(defvar ui:entity-counter nil)
(defvar entity-data (make-hashtable))
(defun ui:entity (scope model)
  (let ((scope-args (car model)))
	 (set! model (cdr model))
	 (set! ui:entity-id ui:next-id)
 	 (let ((data (hashtable-ref entity-data ui:entity-id))) 
		
		(unless data
		  (println 'gen-data)
		  (for-each arg scope-args
						(set! data (cons (cons (car arg) (eval (cadr arg) scope)) data))

						)
		  (hashtable-set entity-data ui:entity-id data)
		  
		  (loop scope-args
				 
				 (set! scope-args (cdr scope-args))
				 ))
		
		(incf ui:next-id 1)
		(let ((prev ui:next-id))
		  (set! ui:next-id (* ui:next-id 1024))
		  ;(println ui:entity-id)
		  (lisp:with-variables scope (cons (cons 'model model) data)

									  '((eval-scoped0 (lisp:get-current-scope!!) model)))
		  (set! ui:next-id prev)
		  ))))


(defun ui:entity-root (scope model)
  (set! ui:next-id 0)
  (eval-scoped0 scope model)
  )
(defvar wing-flap 0.0)
(defvar wing-in -80.0)
(defvar tail-spread 5)
(defvar bird-wing
  '(rgb (1 1 1)
	 (rotate (0 (bind wing-in) (bind wing-flap))
	
	 (right-tile)
	 (offset (0.8 0 0)
	  (rotate (0 0 (bind wing-flap))
		(right-tile)
	 
	  )))))

(defvar bird-model
  '(rgb (1 1 1)
	 ;;head
	 (tile-model-2)
	 ;; eyes
	 (rgb (0 0 0)
	  (offset (0.2 0.1 0.1)
		(scale (0.1 1.0 0.2)
				 (tile-model-2)))
	  (offset (-0.2 0.1 0.1)
		(scale (0.1 1.0 0.2)
				 (tile-model-2)))

	  )

	 
	 ;; beak
	 (offset (0 0 1)
	  (rgb (1 1 0)
		(rotate (0 0 0)
				  (scale (0.5 1.0 1.0)
							(tile-model-2)))))
	 ;;body
	 (offset (0 0 -1)
	  (scale (1.1 1.0 2.0)
		(tile-model-2)))
	 ;; tail
	 (offset (0 0 -2.5)
	  (offset (0 0 1)
	  (for a (range -3.3 1.65 3.3)
	  	(rotate (0 (bind (* tail-spread a)) 0)
				  (offset (0 0 -1.5)
							 (scale (0.5 1.0 2.0)
									  (tile-model-2)))))))
	 ;; right wing
	 (offset (0.4 0 -1)
	  (bird-wing))
	 ;; left wing
	 (offset (-0.4 0 -1)
	  (scale (-1 1 1)
		(bird-wing)))
	 ))
(defvar ground
  '(rgb (0 1 0)
	 (scale (100 0 100)
	  (tile-model-2))
	 (for id (range 100)
	  (ui:entity ((x (math:random -20.0 20.0))
					  (z (math:random -20.0 20.0)))
		(offset ((bind x) 1.0 (bind z))
				  (rgb (1 0 0)
						 (tile-model-2))))
	  
	  )))
(set! model
  '((perspective (1.0 1.0 0.1 1000.0)
	  (depth t
		
		(rgb (1 0 0)
			  (rotate (90 180 0)
						 (offset (0 -20 0)
									(offset (0 -10 0)
									(ground))

						(ui:entity-root
						 (ui:entity ((x 0) (y 0))
						  (bird-model))

						 ))))))))
