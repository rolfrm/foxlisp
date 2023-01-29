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

(set! model '((perspective (1.0 1.0 1.0 1000.0)
					(offset (0 0 -5)
					 (rotate (0 0 0)
					  (translate (-0.5 -0.5 0)
						(rgb (1 0 0)
						 (square2 1.1 2)
						 (square2 2 1.1)
						 (offset (-0.175 -0.175 0)
									(rgb (0 1 0)
										  (square2 1.5 1.5))))
						(rgb (0 0 0)
						 (rgb (0 0 1)
								(square2 1.0 1.0))
						 (text2 "Hello world!")))
						)))))
