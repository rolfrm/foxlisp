(defvar tower

  '(
	(cache :size (256 256)
	(rgb (0.2 0.2 0.2)
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
	  '((ortho (10 10 10)
		 (depth t
		  (rgb (0.2 0 0)
			   (rotate (0.5235987755982988  (bind (/ pi 4.0)) 0)
					   (translate (1 0 0)
								  (ref tower))
					   (translate (10 1.5 1.5)
							(rotate (1 0 0)
					   (cache :size (256 256)
							  	  (rgb (1 1 1)
									   (scale (2 2 2)
											  (translate (0.2 0 0)
														 (rgb (0.2 0 0)
															  (ref cube-2)))
											  (translate (0 0.2 0)
														 (rgb (0 0.2 0)
															  (ref cube-2)))
											  (translate (0 0 0.2)
														 (rgb (0 0.2 0.2)
															  (ref cube-2)))
											  (ref cube-2))))
					   ))
					   (scale (1 1 1)
							  
				(cache :size (256 256)
					   (rgb (0.4 0.8 0.4)
							(scale (100 1 100)
								   (ref upcube)
								   ))
					   (rgb (0.3 07 0.3)
							(for i (1 2 3 4 5)
								 (for j (1 2 3 4 5)
								 (translate ((bind (- (* 2 i) 5)) 0 (bind (- (* 2 j)  5)))
							(scale (1 1.2 1)
								   
										(ref upcube))))))
					   )))))
)))

