(defvar player-rotation 0.0)
(defvar player-forward 0.0)
(defvar player-left 0.0)
(defvar player-x 0.0)
(defvar player-y 5.0)
(defvar player-z 0.0)

(defvar up (vec3 0 1.0 0))
(defvar left (vec3 1 0 0))
(defvar forward (vec3 0 0 1))
(defvar p2 (vec3 0 0 0))
(defvar p3 (vec3 0 0 0))
(defvar wheel-roll 0.0)


(defvar world-sdf '(rgb (1 1 1)
					(rgb (0.2 0.6 0.1) (sphere 1.0))
					(offset (4 0 0)
					 (sphere 1.0)
					 (offset (1.5 0 0)
					  (sphere 1.0))
					 )
					
					(rgb (0.2 0.8 0.1)
					 (aabb 3.5 0.5 3.5)
					 (offset (9 0 0)
					  (aabb 3.5 0.5 3.5))
					 
					  )))
(defvar world-sdf-layer2
  '(rgb (1 1 1)
	(rgb (0.5 0.5 0.1)
	 (soft 0.25
	  (aabb 0.75 0.15 0.75)
	  )
	 )))

(defvar model2 nil)
(defvar view nil)
(set! view (mat4:look-at up (vec3+ (vec3 0 5 0) (vec3-scale up 2.0)) forward))
(set! model2 (mat4:look-at (vec3 0 0 0) (vec3 0 1 0) (vec3 0 0 1)))

(defun game-update(events)
  ;(println model2)
  
  (for-each x events

			(let ((power 0.0))
			  (when (eq (car x) 'key-down)
                (set! power 1.0))
			  (when (eq (car x) 'key-up)
                (set! power -1.0))

			  (when (eq (caddr x) foxgl:key-a)
                (incf player-left (* 1.0 power)))
			  (when (eq (caddr x) foxgl:key-d)
                (incf player-left (* -1.0 power)))
			  (when (eq (caddr x) foxgl:key-w)
                (incf player-forward (* 1.0 power)))
			  (when (eq (caddr x) foxgl:key-s)
                (incf player-forward (* -1.0 power)))
			  
			  )
			)
  (let* ((p (vec3 player-x player-y player-z)))
	(set! p (vec3+ p (vec3-scale left (* 0.1 player-left))))
	(set! p (vec3+ p (vec3-scale forward (* 0.1 player-forward))))
	(set! player-x (vector-ref p 0))
	(set! player-y (vector-ref p 1))
	(set! player-z (vector-ref p 2))
	)
  (let* ((p (vec3 player-x player-y player-z))
		 (g (sdf:dist-gradient world-sdf p 0.1) ) 
		 (d (- (sdf:dist world-sdf p) 0.4)))
	
	(incf player-x (* (vector-ref g 0) d -0.1))
	(incf player-y (* (vector-ref g 1) d -0.1))
	(incf player-z (* (vector-ref g 2) d -0.1))
	(set! up g)
	(set! left (vec3-normalize (vec3:cross up forward)))

	(let ((eye (vec3- (vec3+ p (vec3-scale up (float32 1.0)))
					  (vec3-scale forward 4.0))))
	  (set! view (mat4:look-at eye p (vec3 0 0 1)))
	  
	  )
	(when (or (> (abs player-forward) 0.0)
			  (> (abs player-left) 0.0))
	  (incf wheel-roll 0.1)
	  (set! model2 (mat4:look-at
					(vec3 0 0 0)
					(vec3+ (vec3-scale forward player-forward)
						   (vec3-scale left player-left))
					
					up))
	  (mat4:invert! model2))
	

	))

(defvar sphere '(sdf2 (:size 2 :resolution 0.05)
											 (rgb (1 1 1)
											  (sphere 0.5))))

(defvar wheel
  '(scale (1 1 0.2)
	(rgb (0.2 0.2 0.2)
	 (rotate (0.5 0 45.0)
	  (cube-2)
	  )
	 (rotate (0.5 0 22.5)
	  (cube-2)
	  )
	 (rotate (0.5 0 67.5)
	  (cube-2)
	  )
	 (cube-2)
	 )))

(defvar player-model
  '(rotate (0 90 0)
	(scale (0.2 0.2 0.2)
	 (translate (-2 0 0)
	 (rgb (1 0 0)
	 (translate (-0.5 -0.2 0)
	  (rotate (0 0 (bind (* -50.0 wheel-roll)))
			  (scale (1 1 5.0)
					 (rgb (0.2 0.2 0.2)
						  (rotate (0.5 0 45.0)
								  (cube-2)
								  )
						  (cube-2)
						  ))))
	 (translate (2 0 0)
	  (for x (-1.2 1.9)
		   (for y (-1.5 1.5)
				(translate ((bind x) -0.4 (bind y))
						   (rotate (0 0 (bind (* 30.0 wheel-roll)))
								 (wheel)))))
	  
	  (translate (1.0 0.0 0)
			
	  (scale (5 3 3)
			 (translate (0 0.4 0)
						(rgb (0.8 0.8 0.8)
							 (cube-2)))))
	  (translate (-1 2 0)
				 (scale (1.2 1 3.5)
						(rgb (0.2 0.2 0.2)
							 (cube-2))))
	  
	  ))))))

(defvar player-tform (mat4-identity))
(defvar player-model2 nil)
(defvar root-tform (mat4-identity))
(defvar col-out (cons nil nil))
(defvar id-counter 0)
(defvar closed-id-table (make-hashtable))


(defun register-root (scope model)
  
  (mat4:identity! root-tform)
  (math:*! root-tform root-tform (symbol-value 'current-transform scope))
  (mat4:invert! root-tform)
  (set! id-counter 0)
  )

 (defun register-player (scope model)
  (let* ((tform (symbol-value 'current-transform scope)))
	(math:*! player-tform root-tform tform)
    (mat4:invert! player-tform)
	(eval-scoped scope model)
	(set! player-model2 model)
	))

(defun sdf2-register(scope model)
  (let* ((tform (symbol-value 'current-transform scope))
		 (new-tform (get-matrix)))
	(mat4:identity! new-tform)
	(math:*! new-tform root-tform tform)
	
	(math:*! new-tform player-tform new-tform)

	(set-cdr! col-out nil)
	(set-car! col-out nil)
	(incf id-counter 1)
	(let ((col (sdf:detect-collision '(sphere 0.5) world-sdf-layer2
									 new-tform col-out)))
	  (when col
		(hashtable-set closed-id-table id-counter 1))
	  (unless (hashtable-ref closed-id-table id-counter)
		(eval-scoped scope model))
	  (release-matrix new-tform)
	)))

(set! model
	  '((perspective (1.0 1.0 1.0 1000.0)
		 (depth t
		  (transform (bind view)
		  	(rgb (0 0 1)
				 (translate (0 -10 0)
					(scale (100 1 100)
						   (cube-2)))
				 (register-root)
				 (rgb (1 1 1)
					  (translate ((bind player-x) (bind player-y) (bind player-z))
								 (transform (bind model2)
											(register-player )
										;	(sdf2 (:size 2 :resolution 0.1)
										;		  (sphere 0.5))
										(player-model)
											)
								 )
					  
					  (for pcn (bind (list (cons p2 '(1 0 0)) (cons p3 '(0 1 0))))
						   (vars ((pn (car pcn)))
								 (translate ((bind (vec3-x pn))
											 (bind (vec3-y pn))
											 (bind (vec3-z pn)))
									(rgb (bind (cdr pcn))
										 (scale (0.2 0.2 0.2)
												(sphere 1.0))))))
										
					  (for z (0.4 -0.4)
						   (for x (-2.1 0.0 2.1 )
								(for y (-2.1 0.0 2.1)
									 (translate ((bind x) (bind z) (bind y))
												(sdf2-register
												 (sdf2 (:size 5 :resolution 0.1) (bind world-sdf-layer2))
												 )
												))))
					  
					  (sdf2 (:size 25 :resolution 0.1) (bind world-sdf))
						  
					  )))))))
