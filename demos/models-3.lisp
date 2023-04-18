
(defvar door-1-model
  '(rgb (0.5 0.4 0.4)
	 (scale (1.4 2.2 0.2)
	  (upcube))
	 (rgb (0.8 0.7 0.2)
	  (offset (0.5 1.0 0.01)
		(scale (0.2 0.1 0.2)
				 (upcube))))

	 ))
(defvar window-1-model
  '(rgb (0.5 0.5 0.9)
	 (scale (1.4 1.4 0.2)
	  (upcube))))
(defvar flowerbed-1-model
  '(bake2
	 (rgb (0.5 0.4 0.35)
	 (scale (1.0 0.2 0.2)
	  (upcube))
	 (scale (0.9 0.21 0.19)
	  (rgb (0.3 0.2 0.2)
		(upcube)))
	 (for i (-2 -1 0 1 2)
	  (offset ((bind (* 0.18 i)))
		(rgb (0.15 0.7 0.15)
			  (scale (0.05 (bind (* 0.2 (random 1.0 4.0))) 0.05)
						(upcube)))))
	 
	 )))

(defvar house-1-model
  '(rgb (0.6 0.4 0.35)
	 (scale (4 3 4)
	  (upcube)
	  (offset (0 1 0)
		(scale (1.2 0.5 1.2)
				 (rgb (0.5 0.4 0.3)
						(pyramid))))
	  
	  )
	 
	 (offset (0 0 2)
	  (offset (1 0 0.4)
		(scale 1.9
				 (flowerbed-1-model)))
	  (offset (-1 0 0)
		(door-1-model))
	  (offset (1 1 0)
		(window-1-model)))
	 (rotate (0 90 0)
	  (offset (-0.5 1 2)
		(window-1-model)))
	 

	 ))

(defvar flower-floor-model
  '(bake2
	 (for i (range 300)
	  (offset ((bind (random -100.0 100.0)) 0.01 (bind (random -100.0 100.0)))
		(scale (bind (+ 4.0 (random 0.2 1.5)))
				 
				 (rgb (0.3 (bind (random 0.4 0.7)) (bind (random 0.2 0.3)))
						(rotate (0 (bind (random 0 180)) 0 )
						(scale (0.05 0.05 0.05)
								 (tile-model))
						(offset (0.1 0 0.05)
								  (scale (0.05 0.05 0.05)
								 (tile-model)))
						(offset (0.0 0 0.1)
								  (scale (0.05 0.05 0.05)
								 (tile-model)))

						)

		))))))

(defvar rock-model-1
  '(bake2
	 (rgb (0.5 0.5 0.52)
	  (offset (0 0 0)
		(scale 4
				 (upcube)))
	  (offset (2 0.5 0)
		(scale 3
				 (rotate (40 40 40)
							(upcube))))
	  (offset (-2 0.5 0)
		(scale 3
				 (rotate (40 40 40)
							(upcube))))
	  (offset (0 0.5 -2.5)
		(scale 3
				 (rotate (40 40 40)
							(upcube))))

	  )))
