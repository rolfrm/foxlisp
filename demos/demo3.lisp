(defvar horse
    '(horsebody 
        (scale (0.1 1.0 0.1)
            (upcube))
        (offset (0 1 0)
        (scale (1.0 1.0 2.0) 
        (upcube))
    
    )))

(set! model
	  '((perspective (1.0 1.0 0.1 100.0)
		 (depth t
		  (rgb (1 0 0)
		      (offset (0 0 -10)
		      (rotate (0.0 0.0 (bind real-time))
   		         (horse)
		      )))
)))))