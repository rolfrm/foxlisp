(load "models-2.lisp")
(defvar camera-offset '(0 16 20))
(defvar wizard-offset '(3 0 5))
(defvar wizard-walk 0.0)
(defvar wizard-angle 0.0)


(defun game-update (events)
  (when (key-down? foxgl:key-w)
	 (incf wizard-walk 0.1)
	 (set! wizard-angle 180)
	 (set! wizard-offset
			 
			 (list (car wizard-offset)
					 (cadr wizard-offset)
					 (- (caddr wizard-offset) 0.1))))
  (when (key-down? foxgl:key-a)
	 (incf wizard-walk 0.1)
  (set! wizard-angle 90)
  (set! wizard-offset
			 (list (- (car wizard-offset) 0.1)
					 (cadr wizard-offset)
					 (caddr wizard-offset))))
(when (key-down? foxgl:key-d)
  (incf wizard-walk 0.1)
  (set! wizard-angle -90)
  (set! wizard-offset
			 (list (+ (car wizard-offset) 0.1)
					 (cadr wizard-offset)
					 (caddr wizard-offset))))
  (when (key-down? foxgl:key-s)
	 (incf wizard-walk 0.1)
	 (set! wizard-angle 0)
	 (set! wizard-offset
			 (list (car wizard-offset)
					 (cadr wizard-offset)
					 (+ (caddr wizard-offset) 0.1))))

  (when events
	 (println events))
  )

(set! model
	  '((perspective (1.0 1.0 0.1 1000.0)
		 (depth t
		  (rgb (1 0 0)
			 (rotate (40 0 0)
			  (offset ((bind (- (+ (car wizard-offset)
										  (car camera-offset))))
						  (bind (- (+ (cadr wizard-offset)
										  (cadr camera-offset))))
						  (bind (- (+ (caddr wizard-offset)
										  (caddr camera-offset))))
						  )
				
			  (offset (-500 -2 -500)
			 (rgb (0.35 0.7 0.25)
			  (scale (1000 1 1000)
						(tile-model))))

			(offset (0 -5 -500)
			 (rgb (0.7 0.7 1)
			  (scale (1000 100 100)
						(upcube))))
		   
			(offset (-500 50 -500)
			 (rgb (0.5 0.5 1)
			  (scale (1000 1 1000)
						(tile-model))))

				(vars ((run-cycle (* 2 wizard-walk)))
						(offset (bind wizard-offset)
								  (rotate (0 (bind wizard-angle) 0)
								  (scale 0.4
											(offset (0 1 0)
								  (wizard-model)
								  (offset (5 0 0)
											 (cat-model)
											 )
								  
								  )))))
		    
			 (scale 1.0
		       (rotate (0.0 0 0.0)
				  (for i (-30 0 30);(-140 -120 -100 -80 -60 -40 -20 0 20 40 60 80 100 120 140)
				 		 (for j (-30 0 30);(-140 -120 -100 -80 -60 -40 -20 0 20 40 60 80 100 120 140)
								(offset ((bind i) 0 (bind j))
										  (rotate (0 (bind (* 0 (+ i j))) 0) 
													 (cherry-tree 6 (bind (+ i (* j 100))))))))
				  (offset (0 0 0)
				  (vars ((run-cycle (* 2 real-time)))
						  (rotate (0 (bind (* 10.0 run-cycle)) 0)
									 (offset (4 0 0)
   											(horse))))
				  (vars ((run-cycle (* 2 real-time)))
						  (rotate (0 2 0)
									 (offset (8 0 0)
												(scale (0.3 0.3 0.3)
														 (rotate (0 (bind (* 3 real-time)) 0)
																	(elephant))))))
				  (vars ((run-cycle (* 2 real-time)))
						  (rotate (0 4 0)
									 (offset (12 0 0)
												(scale (0.3 0.3 0.3)
														 (rotate (0 (bind (* 5 real-time)) 0)
   																(duck-toy))))))
					
							 
							 )
		      )))))
))))

(defvar model2
	  '(perspective (1.0 1.0 0.1 1000.0)
		 (depth t
		  (rgb (1 0 0)
			(offset (0 0 -5)
			 (rotate (0 (bind real-time) 0)
			  (wizard-model)))))))
