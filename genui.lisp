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

(defvar calculated-width 0.0)
(defvar calculated-height 0.0)

(set! model '((perspective (1.0 1.0 1.0 1000.0)
					(translate (0 0 -5)
					 (rotate (0 0 0)
								(scale (1 -1 1)
										

										 
								(translate (-0.0 -0.0 0)
											  (translate (2 0 0)
											  (rgb (1 0 0)
						 (square 1.1 2)
						 (square 2 1.1)
						 (offset (-0.175 -0.175 0)
									(rgb (0 1 0)
										  (square 1.5 1.5))))
						(rgb (0 0 0)
							  (rgb (0 0 1)
									 (vars ((w 1.5) (h 1.2))
								(square (bind w) (bind h))))
							  (grid
								(rgb (1 1 1) (square (bind (* 0.01 (car ui:desired-size)))
														 (bind (* 0.01 (cdr ui:desired-size )))))
								(text "Hello world -!\n1122\n11\n11\nxxxx\n\n\n\n\123")
								
								)
							  ))

											  (offset (-1.5 -1.5 0.0)
											  (scale (0.5 0.5 0.5)
											  (for i (0 2.2 4.4)
													 (for j (0 2.2 4.4)
											  (translate ((bind i) (bind j) 0)
											  (rgb (1 1 1)
													 (rotate (0 0 (bind (* real-time 3.0)))
																(star)
																))											  
															 )

															))
											  )))
						))))))
