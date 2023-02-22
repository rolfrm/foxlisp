(defvar tower

  '(
	(cache :size (256 256)
	       :region (20 20)
	(rgb (0.2 0.2 0.23)
	 (translate (0 1.1 0)
	  (for i (0 1 2 3 4 5 6)
		   (rgb (0.5 0.3 0.3)
				  (translate (0 (bind (* 1.5 i)) 0)
							 (scale (2 0.2 2)
									(rotate (0 (bind (* pi 0.125 i) 0))
											(ref cube-2))))))
	  (scale (1 10 1)
			 (ref upcube)))))))
	


(set! model
	  '((ortho (20 20 100)
		 (depth t
		  (rgb (0.2 0 0)
			   (rotate (0.5235987755982988  (bind (/ pi 4.0)) 0)
					   (for i (0 -1 -2 -3 -4 1 2 3 4)

					   (translate ((bind (* 5 i)) (bind (* 2.0 (sin (* 0.2 real-time)))) 0)
								  (rotate (0 0 0) 
										  (ref tower)))
					   (translate (0 (bind (* 2.0 (sin (* 0.2 real-time)))) (bind (* 5 i)))
								  (rotate (0 0 0) 
										  (ref tower))))
					   
					   (scale (1 1 1)
							  (let2 ((phase (* 0.2 (math:floor (* 5 (math:mod real-time (* 2 pi)))))))
							  (cache :size (256 256)
									 :key (bind phase)
					   (rgb (0.4 0.8 0.4)
							(scale (100 1 100)
								   (ref upcube)
								   ))
					   (rgb (0.3 07 0.3)
							(for i (-6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10)
								 (for j (-6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10)
								 (translate ((bind (- (* 3 i) 5)) 0 (bind (- (* 3 j)  5)))
							(scale (1 (bind (+ (* (+ (cos (+ i phase)) 1) (+ (sin (+ j phase)) 1) 1.0) 1.2)) 1)
								   
								   (ref upcube))))))
					   )))))
)))

