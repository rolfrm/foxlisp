(defvar sphere
  '(sdf2 (:size 2 :resolution 0.2)
	 (rgb (1 1 1)
	  (sphere 0.5))))

(defvar rabbit
  '(rgb (1.0 0 0)
	(rotate ((bind (* 10.0 real-time)) 0.0 0.0)
	(translate (0.5 0 0)
		   (rgb (0 0 1.0)
			(sphere)))
	(translate (0 0.5 0)
		   (rgb (0 1.0 0.0)
			(sphere)))
	(translate (0.0 0.0 0.5)
		   (rgb (1.0 0.0 0.0)
			(sphere)))
	(translate (0.0 0.0 -0.5)
		   (rgb (1.0 1.0 0.0)
			(sphere)))
	 )))

(defvar star
  '(bake2
	 (scale (0.2 0.2 0.2)
	  (for j (0.0 0.5 1 1.5 2.0 2.5 3.0)
		(for i (0 60 120 180 240 300)
			  (rotate (0 0 (bind i))
						 (translate ((bind j) (bind j) 0)
										(offset ((bind (* 0.75 j)) 0 0)
												  (square 0.4 6.37)
												  )))
			  )))))


(defvar stars '(scale (0.25 0.25 0.25)
					 (for i (-11.0 -8.8 -6.6 -4.4 -2.2 0.0 2.2 4.4 6.6 8.8 11.0)
					  (for j (-11.0 -8.8 -6.6 -4.4 -2.2 0.0 2.2 4.4 6.6 8.8 11.0)
						(translate ((bind (+ (* 1.0 i) (* 0.51 j))) (bind (* 0.9 j)) 0)
									  (rgb (0 0 0.0)
											 (rotate (0 0 0)
														(star)
														))											  
									  )
						))
					 ))

(defvar baked-stars '(bake2 (stars)))


(defvar calculated-width 0.0)
(defvar calculated-height 0.0)

(set! model '((perspective (1.0 1.0 1.0 1000.0)
					(translate (0 0 -5)
					 (rotate (0 0 0)
								(scale (1 -1 1)
										
										 (translate (-1.7 -2.0 0)
														(translate (-0.8 -0.5 0)
				
						(rgb (0 0 0)
							  (grid
								(offset (0.5 0.5 0)
								(rgb (1 1 1) (square (bind 4)
															(bind 4))))
								(vars ((max-width 100.0))
										(text "Hello world -!\n1122\n11\n11\nxxxx\n\n\n\n\123\n\n\n\n\n\n\n\n\n\n\n\n\11---111"))
								
								)
							  ))
														(offset (1.7 2 0.0)
									(rotate (0 0 (bind real-time))	 
											  (baked-stars))
																  (offset (50 0 0)
																  (rgb (0 0 0)
																		 (vars ((max-width 200))
											 (text (bind '(1 2 3 4 5 6 7 8 9 10 11 22 33))))))
									  ))))
						))))
