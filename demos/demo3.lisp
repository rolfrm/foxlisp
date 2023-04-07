(defvar leg-rotation-1 0)
(defvar leg-rotation-2 0)
(defvar horse-leg
  '(offset (0 1 0)
	 (rotate ((bind leg-rotation-1) 0 0)
	  (scale (0.1 0.5 0.1)
	  
		(downcube))
	  (offset (0 -0.5 0)
		(rotate ((bind leg-rotation-2) 0 0)
		 (scale (0.1 0.5 0.1)
	 	  (downcube)))
		))))
(defvar run-cycle 0.0)
(defvar horse
  '(rgb (0.7 0.4 0.3)
	 (horsebody
	  (vars ((leg-rotation-1 (* 50 (sin run-cycle)))
				(leg-rotation-2 (+ (* 40 (sin run-cycle)) 40))
				)
      (offset (-0.2 0 -1) (horse-leg))))
	 (vars ((leg-rotation-1 (* 50 (sin (+ 10 run-cycle))))
				(leg-rotation-2 (+ (* 40 (sin (+ 10 run-cycle))) 40))
				)
     (offset (0.2 0 -1) (horse-leg)))
	 (vars ((leg-rotation-1 (* 45 (sin run-cycle)))
				(leg-rotation-2 (+ (* 40 (sin run-cycle)) 40))
				)
     (offset (-0.2 0 1) (horse-leg)))
	 (vars ((leg-rotation-1 (* 45 (sin (+ 10 run-cycle))))
				(leg-rotation-2 (+ (* 40 (sin (+ 10 run-cycle))) 40))
				)

     (offset (0.2 0 1) (horse-leg)))
      (offset (0 1 0)
		 (offset (0 0.9 -0.95)
		  (rotate (0 0 (bind (* -10 (sin run-cycle))))
		  
		  (rotate (-150 0.0 00)
			(scale (0.2 1.0 0.2)
          (rgb (0.8 0.6 0.6)
			  (upcube))))))
        (scale (0.7 1.1 2.0) 
			(upcube))
		 (offset (0 -0.04 0)
        (scale (0.75 1 1.0)
			(rgb (0.8 0.6 0.6)
			 (upcube))))
		 (offset
		  (0 0.8 1)
		  (rotate (0 0 (bind (* 10 (sin run-cycle))))
		  (rotate (30 0 0)
		    (scale (0.5 0.6 1.0)
			(upcube)))
		  (rgb (0 0 0)
			(translate (0.25 0.2 0.30)
			 (rotate (-25 0 0)
			 (scale (0.05 0.25 0.1)
			  (upcube))))
			 (rotate (-25 0 0)
			 (translate (-0.35 0.2 0.20)
			 (scale (0.05 0.25 0.1)
			  (upcube))
			 )))
		  )
		  ))))

(defvar cherry-tree-color '(0.5 0.4 0.4))
(defvar cherry-flower-color1 '(1 0.8 0.8))
(defvar cherry-flower-color2 '(1.0 0.75 0.75))
(defvar cherry-tree2
  '(vars ((x (println (car args))))
		  (upcube)))
(defvar cherry-tree
  '(bake2
	 (rgb (bind cherry-tree-color)
	 (vars ((it (- (or (car args) 6) 1)))
		(scale (1 3 1)
	  (upcube))
	  (offset (0 3 0)
		(rotate (0 0 30)
		 (scope:if (> it 0)
					  (scale (0.9 0.9 0.9)
								(cherry-tree (bind it))))
		 (scope:if (< it 3)
													 ;(cherry-tree2 (bind it))
					  (scale (1.5 1.5 1.5)
								(rgb (bind cherry-flower-color2)
									  (upcube)
									  )))

		 )
		
		(rotate (70 -30 -90)
		 (scope:if (> it 0)
					  (scale (0.9 0.9 0.9)
						 ;(cherry-tree2 (bind it))
						 (cherry-tree (bind it)))
													 ;(cherry-tree (bind (- it 1)))
					  )
		 (scope:if (< it 3)
													 ;(cherry-tree2 (bind it))
					  (scale (1.5 1.5 1.5)
								(rgb (bind cherry-flower-color1)
											  (upcube))					
					  ))
		 )
			(rotate (20 2 30)
		 (scope:if (> it 0)
					  (scale 0.9
						 ;(cherry-tree2 (bind it))
						 (cherry-tree (bind it)))
													 ;(cherry-tree (bind (- it 1)))
					  )
		 (scope:if (< it 3)
													 ;(cherry-tree2 (bind it))
					  (scale (1.5 1.5 1.5)
								(rgb (bind cherry-flower-color1)
											  (upcube))					
					  ))
		 )


		)))))

(set! model
	  '((perspective (1.0 1.0 0.1 1000.0)
		 (depth t
		  (rgb (1 0 0)
			(offset (-500 -2 -500)
			 (rgb (0.35 0.7 0.25)
			  (scale (1000 1 1000)
						(tile-model))))

			(offset (0 -5 -500)
			 (rgb (0.7 0.7 1)
			  (scale (1000 100 100)
						(upcube))))
		   
			(offset (-500 10 -500)
			 (rgb (0.5 0.5 1)
			  (scale (1000 1 1000)
						(tile-model))))
			
		   (offset (0 -2 -20)
			 (scale (0.9 0.9 0.9)
		       (rotate (0.0 (bind (* -5 real-time)) 0.0)
				  (for i (-40 0 40)
				 		 (for j (-40 0 40)
								(offset ((bind i) 0 (bind j))
										  (rotate (0 (bind (* 20 (+ i j))) 0) 
													 (cherry-tree)))))
				  (offset (0 0 0)
				  (vars ((run-cycle (* 2 real-time)))
						  (rotate (0 (bind (* 10.0 run-cycle)) 0)
									 (offset (4 0 0)
   								 (horse)))))
		      ))))
))))
