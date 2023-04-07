(load "models-2.lisp")
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
