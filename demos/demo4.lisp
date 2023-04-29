(load "models-2.lisp")
(load "models-3.lisp")
(load "table.lisp")
(println 'load-demo-4)


(defvar ui:entity-id nil)
(defvar ui:next-id 0)
(defvar ui:entity-counter nil)
(defun ui:entity (scope model)
  (set! ui:entity-id ui:next-id)
  (incf ui:next-id 1)
  (let ((prev ui:next-id))
	 (set! ui:next-id (* ui:next-id 1024))
	 (println ui:entity-id)
	 (eval-scoped0 scope model)
	 (set! ui:next-id prev)
  ))


(defun ui:entity-root (scope model)
  (set! ui:next-id 0)
  (eval-scoped0 scope model)
  )

(set! model
  '((perspective (1.0 1.0 0.1 1000.0)
	  (depth t
		
		(rgb (1 0 0)
			 (offset (0 0 -50)
						(ui:entity-root
						 (scale 2.0
								  (rotate ((bind real-time) (bind real-time) 0)
											 (for x (range 0 10)
													(ui:entity
													 (offset ((bind (* x 2.0)) 0 0) 
														(wizard-model)
														(ui:entity
														 (rgb (0 1 0)
																(upcube))
													  )

											 ))))))))))))
