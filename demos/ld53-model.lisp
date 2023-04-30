(load "models-2.lisp")
(load "models-3.lisp")
;(load "table.lisp")

(defun interpolate-angle(now target turn-speed)
  (let ((now (mod now 360.0))
		  (target (mod target 360.0)))
	 (let ((diff (- target now)))
		(when (> (abs diff) 180)
		  (set! target (+ target (if (> target now) -360.0 360.0)))

		  (set! diff (- target now))
		  )
		(mod (+ now (* (sign diff) (min turn-speed (abs diff)))) 360))))

(defmacro with-clist(clist &rest body)
  `(lisp:with-variables! (lisp:get-current-scope!!) ,clist
	  (quote (quote ,@body))
	  
  ))

(defvar dec-to-rad (* pi (/ 2.0 360.0))) 
(defvar ui:entity-id nil)
(defvar ui:next-id 0)
(defvar ui:entity-counter nil)
(defvar entity-data (make-hashtable))
(defun ui:entity-data (id)
  (hashtable-ref entity-data id)
  )
(defun ui:entity (scope model)
  (let ((scope-args (car model))
		  (id nil))
	 (set! model (cdr model))
	 (unless (cons? scope-args)
		(set! id scope-args)
		(set! scope-args (car model))
		(set! model (cdr model))
		)
	 (set! ui:entity-id (or id ui:next-id))
 	 (let ((data (ui:entity-data ui:entity-id))) 
		(unless data
		
		  (for-each arg scope-args
						(set! data (cons (cons (car arg) (eval (cadr arg) scope)) data))

						)
		  
		  (hashtable-set entity-data ui:entity-id data)
		  
		  (loop scope-args
				 
				 (set! scope-args (cdr scope-args))
				  ))
		
		(incf ui:next-id 1)
		(let ((prev ui:next-id))
		  (set! ui:next-id (* ui:next-id 1024))
		  (lisp:with-variables scope (cons (cons 'model model) data)

									  '((eval-scoped0 (lisp:get-current-scope!!) model)))
		  (set! ui:next-id prev)
		  ))))


(defun ui:entity-root (scope model)
  (set! ui:next-id 0)
  (eval-scoped0 scope model)
  )

(defvar phase-offset 0.0)
(defun move-circle (scope model)
  (with-clist (ui:entity-data ui:entity-id)
	 (let ((radius (car model))
			 (phase (+ phase-offset real-time)))

		(set! x (+ x (* 0.025 (sin phase) radius)))
		(set! z (+ z (* 0.025 (cos phase) radius)))
		(set! angle (* -25.0 real-time))
		)

  ))


(defvar bee-wing-1
  '(rotate (15 0 0)
	 (offset (1.5 0 0)
	  (scale (1.5 0.1 0.9)
	  
		(sphere1)))))
(defvar bee-1
  '(scale 0.2
	 (rgb (0.3 0.2 0.2)
	 (for x (range -1 0.5 1)
	  (offset (0 0 (bind x))
		
	  (scale (1.1 1 0.2)
		(rgb (1 1 0)
			  (sphere1)
			  ))))
	 ;;stinger
	 (offset (0 0 0.5)
	  (scale (0.1 0.1 2.0)
		(sphere1)))
	 ;; body
	 (scale (1 1 2.0)
	  (sphere1))

	 ;;wings

	 (offset (0. 0 0)
	  (rotate (0 0 (bind (+ 20 (* 40 (sin (* 10.0 real-time))))))
		(bee-wing-1)))
	 
	 (offset (-0.5 0 0)
	  (rotate (0 0 (bind (+ -20 (* 40 (sin (* -10.0 real-time))))))
	
		(scale (-1 1 1)
				 (bee-wing-1))))
	 )))

(defvar wing-flap 0.0)
(defvar wing-in -80.0)
(defvar tail-spread 5)
(defvar walk-cycle 0.0)
(defvar show-feet t)
(defvar bird-wing
  '(rgb (1 1 1)
	 (rotate (0 (bind wing-in) (bind (* 15 (sin (* 1.5 pi wing-flap)))))
	  (offset (-0.25 0 0)
		(scale (1.5 1.0 1.0)
				 (right-tile))
	  (offset (1.4 0 0)
		(rotate (0 0 (bind (* 15 (sin (* 1.5 pi wing-flap)))))
				  (offset (-0.25 0 0)
							 (scale (1.5 1.0 1.0)
									  (right-tile))
	 
							 )))))))
(defvar bird-feet
  '(rgb (0.7 0.5 0.1)
	 (rotate (0 30 0)
	 (scale (0.2 1.0 1.0)
	  (z-tile))
	 (rotate (0 15 0)
	  (scale (0.2 1.0 1.0)
		(z-tile)))
	 (rotate (0 -15 0)
	  (scale (0.2 1.0 1.0)
		(z-tile)))
	 )))
(defvar blink 1.0)
(defvar bird-model
  '(offset (0 0 1)
	 
	 (rgb (1 1 1)
	 ;;head
	 (tile-model-2)
	 ;; eyes
	 (rgb (0 0 0)
	  (offset (0.2 0.1 0.1)
				 
		(scale (0.1 1.0 (bind (* 0.2 (- 1.0 blink))))
				 (tile-model-2)))
	  (offset (-0.2 0.1 0.1)
		(scale (0.1 1.0 (bind (* 0.2 (- 1.0 blink))))
				 (tile-model-2)))

	  )

	 
	 ;; beak
	 (offset (0 0 1)
	  (rgb (1 1 0)
		(rotate (0 0 0)
				  (scale (0.5 1.0 1.0)
							(tile-model-2)))))
	  (rotate (0 0 (bind (* 15 (sin (* 5 walk-cycle)))))
	  ;;body
	 (offset (0 0 -1)
	  (scale (1.1 1.0 2.0)
		(tile-model-2)))
	 ;; tail
	 (offset (0 0 -2.5)
	  (offset (0 0 1)
	  (for a (range -3.3 1.65 3.3)
	  	(rotate (0 (bind (* tail-spread a)) 0)
				  (offset (0 0 -1.5)
							 (scale (0.5 1.0 2.0)
									  (tile-model-2)))))))
	  ;; right wing
	  
	 (offset (0.4 0 -1)
	  (bird-wing))
	 ;; left wing
	 (offset (-0.4 0 -1)
	  (scale (-1 1 1)
				(bird-wing)))

		(scope:if show-feet
	  ;; left foot
	  (offset (-0.5 -0.8 (bind (+ -1.5 (* 0.4 (sin (* 5.0 walk-cycle))))))
		(rgb (0.7 0.5 0.1)
			  (bird-feet)))

	  ;; right foot
	  (offset (0.5 -0.8 (bind (+ -1.5 (* 0.4 (sin (+ (* 5.0 walk-cycle) 3.14 ))))))
				 (scale (-1 1 1)
		(rgb (0.7 0.5 -0.01)
			  (bird-feet))))

	  ))
	  )))

(defvar taken (make-hashtable))
(defvar object-body (make-hashtable))
;(println object-body)
(defun ui:body (scope model)
  (hashtable-set object-body ui:entity-id model)
  )

(defvar ground
  '(rgb (0.2 0.9 0.2)
	 (scale (200 0 200)
	  (tile-model-2))
	 (for id (range 10)
	  
	  (ui:entity ((x (math:random -20.0 20.0))
					  (y 0.0)
					  (z (math:random -20.0 20.0))
					  (hidden nil))
													 ;(body (takeable ui:entity-id)))
		(scope:if (not hidden)
		(ui:body (sphere 1.0))
		(offset ((bind x) 1.0 (bind z))
				  (bake2
				  (rgb (0.4 0.8 0.4)
						 (tile-model-2))))
	  
		)))))

(defvar ground-baked
  '(bake2 (ground)))

(defvar letter-model
  '(rgb (0.9 0.8 0.2)
	 (tile-model-2)))

(defvar space-down-event '(key-down key 32))
(defvar free-fly t)
(defun game-update (events)
  (let ((player-stats (ui:entity-data :player))
		  (space-down (key-down? foxgl:key-space))
		  )
	 (when player-stats
		(with-clist player-stats
		  (set! blink (if (< (abs (- 25 (mod real-time 50.0))) 0.25) 1.0 0.0 ))
		  ))
	 
	 (when player-stats
		(let ((z (clist-get player-stats 'z))
				(y (clist-get player-stats 'y))
				(x (clist-get player-stats 'x))
				(angle (clist-get player-stats 'angle))
				(animation-time (cdr (clist-get player-stats 'animation-time)))
				(animation (cdr (clist-get player-stats 'animation)))
				(energy (clist-get player-stats 'energy))
				)

		  (when (< (cdr y) 0.001)
			 (set! animation :walk)
			 (set-cdr! (clist-get player-stats 'animation) animation)
					 
			 )
		  (when (eq animation :fly)
			 (when (> animation-time 0.0)
				(when (or (> (cdr energy) 0.04) free-fly)
				  (set-cdr! y (+ (cdr y) 0.04)))
			 	(incf animation-time 0.04)
				(set-cdr! energy (max 0.0 (- (cdr energy) 0.01)))
				)

			 (when (> animation-time 2.0)
				(set! animation-time 0.0))
			 (when (and (< animation-time 0.02) (not space-down))
				(set-cdr! y (- (cdr y) 0.04))
			 
				)
			 )
		  (when space-down
			 (when (or (> (cdr energy) 0.04) free-fly)
				
			 	 (set! animation :fly)
				 (set-cdr! (clist-get player-stats 'animation) animation)
				 
				(set-cdr! y (+ (cdr y) 0.01))
			 (set! animation-time (+ 0.01 animation-time))))
		  (set-cdr! (clist-get player-stats 'animation-time) animation-time)
		  (let ((v-x 0.0)
				  (v-y 0.0)
				  (v-z 0.0)
				  (v-angle 0.0))
			 (when (key-down? foxgl:key-w)
				(set! v-z 0.1))
			 (when (key-down? foxgl:key-a)
				(set! v-x 0.1))
			 (when (key-down? foxgl:key-d)
				(set! v-x -0.1))
			 (when (key-down? foxgl:key-s)
				(set! v-z -0.1))
			 (when (and (eq v-x 0.0) (eq v-z 0.0) (eq animation :fly))
				(set! v-x (- (sin (* (cdr angle) dec-to-rad))))
				(set! v-z (cos (* (cdr angle) dec-to-rad)))
				)
			 (let ((absv (math:sqrt (+ (* v-z v-z) (* v-x v-x)))))
				(when (> absv 0)
				  (when (eq animation :walk)
					 (incf animation-time 0.04)
					 (set-cdr! (clist-get player-stats 'animation-time) animation-time)
		  			 )
				  
				  (let ((target-angle (/ (math:atan (- v-x) v-z) dec-to-rad)))
					 (set-cdr! angle (interpolate-angle (cdr angle) target-angle 10))					 (set! v-x (* 0.2 (/ v-x absv)))
					 (set! v-z (* 0.2 (/ v-z absv)))
					 (set-cdr! x (+ v-x (cdr x)))
					 (set-cdr! z (+ v-z (cdr z)))
					 )
				  )
				
				)))
		(let ((player-body (ui:entity-data :player))
				(mat1 (get-matrix))
				(mat2 (get-matrix))
				(body (hashtable-ref object-body :player))

				)

		  (with-clist player-body
			 (let ((px x)
					 (py y)
					 (pz z))
				(math:translate! mat1 px py pz)
				(hashtable-iter object-body
									 (unless (eq key :player)
									 (let ((object-body (ui:entity-data key))
											 (col-out (cons nil nil)))
										(with-clist object-body
										  (unless hidden
										  (mat4:identity! mat2)
										  (math:translate! mat2 x y z)
										  (let ((col (sdf:detect-collision2 body value mat1 mat2 col-out)))
											 (when col
												(set! hidden t)
												(incf energy 1.0)
										
											 )
										  ))
										))))
				)
			 )
		  (release-matrix mat1)
		  (release-matrix mat2)
		  
		  )

		)))

(defvar cloud-1
  '(rgb (0.9 0.9 1)
	 (sphere1)
	 (offset (0.7 0 0.1)
	  (scale 1.2
	  (sphere1))
	  (offset (1.5 0 -0.1)
		(scale 1.5
				 (sphere1))))))

(defvar skylevel
  '(for id (range 10)
	  
	  (ui:entity ((x (math:random -20.0 20.0))
					  (y y0)
					  (z (math:random -20.0 20.0))
					  (hidden nil))
		
		(scope:if (not hidden)
					 (ui:body (sphere 1.0))
					 (offset ((bind x) (bind y) (bind z))
								(bake2
								 (rgb (0.9 0.8 0.3)
										(tile-model-2))))
	  
		))))



(defun get-data (id sym)
  (cdr (clist-get (ui:entity-data id) sym))
  )

(defvar game:ui
  '(ortho (10 10 10)
  (offset (-9 8 -4)
	(rgb (1 0 0)
	 (scale ((bind (+ 0.1 (or (get-data :player 'energy) 0))) 1 1)
	  (offset (0.5 0 0)
		(upcube))
	 )))))

(set! model
  '((perspective (1.0 1.0 0.1 1000.0)
	  (depth t
		(vars ((player-y (or (get-data :player 'y) 0)))
		(rgb (1 0 0)
			  (rotate (90 180 0)
						 (ui:entity-root
						  (offset (0 -20 0)
									 (offset ((bind (- (or (get-data :player 'x) 0)))
												 (bind (- (or (get-data :player 'y) 0)))
												 (bind (- (or (get-data :player 'z) 0))))
												
												(offset (0 -2 0)
														  (ui:entity :ground ((x 0))
																		 (ground)))
												
												
											 
									
												(ui:entity :player ((x 0)
																		  (y 0) (z 0)
																		  (angle 0)
																		  (animation :fly)
																		  (animation-time 0.0)
																		  (blink 0.0)
																		  (energy 0.0)
																		  )
												  (ui:body (sphere 3.0));(aabb 0.5 0.2 1.0))
		

												  
												  (vars ((wing-flap (if (eq animation :walk) 0.0 animation-time))
															(wing-in (if (eq animation :walk) -80 0))
															(tail-spread (if (eq animation :walk) 3 8))
															(walk-cycle (if (eq animation :walk) animation-time 0.0))
															(show-feet (eq animation :walk))
															

															)
												  
										(offset ((bind x) (bind y) (bind z))
												  (rotate (0 (bind angle) 0)
												  (bird-model))))

												  )
									(ui:entity :letter ((x 10) (y 0) (z -5))
												  (offset ((bind x) (bind y) (bind z))
															 (letter-model)))

												(ui:entity ((x 0) (z 3))
															  (offset ((bind x) 0 (bind z))

																		 (scale 2.0
																		 (rgb (0.6 0.5 0.2)
																		 (cylinder))

																				  (scope:if (> player-y 2)
																				  (rgb (0.3 0.6 0.2)
																				(offset (-0.6 0.5 0.3)
																						  (scale 1.5
																									(upcube)))
																		 																				(offset (0.6 0.4 -0.2)
																																								  (scale 1.4
																																											(upcube))))


																				  ))))

												;; todo: there is a bug if the same entity has a different number of arguments and then calls e.g move-circle.
												;; probably due to the optimizer.
												(ui:entity :bee ((x 10) (y 1.0) (z 10) (angle 10) (phase-offset 0))
															  (move-circle 10.0)
															  (offset ((bind x) (bind y) (bind z))
																		 (rotate (0 (bind angle) 0)
																		 (bee-1)))
															  )

												(ui:entity :bee-2 ((x 7) (y 1.0) (z 7) (angle 10)
																				 (phase-offset 5))
															  (move-circle 10.0)
															  (offset ((bind x) (bind y) (bind z))
																		 (rotate (0 (bind angle) 0)
																		 (bee-1)))
															  )

												(ui:entity :bee-3 ((x 7) (y 1.0) (z -7) (angle 10)
																				 (phase-offset 14))
															  (move-circle 10.0)
															  (offset ((bind x) (bind y) (bind z))
																		 (rotate (0 (bind angle) 0)
																		 (bee-1)))
															  )

												(ui:entity  ((y0 10))
															  (scope:if (> player-y (- y0 5.0))
															 				(skylevel))
																)
												(ui:entity  ((y0 20))
															  (scope:if (> player-y (- y0 5.0))
															 				(skylevel))
																)
												(ui:entity  ((y0 30))
																(scope:if (> player-y (- y0 5.0))
															 				 (skylevel))
																)
												(scope:if (> player-y 5.0)
															 
															 (ui:entity ((x 0 ) (z 0))
												  (offset ((bind x) 8 (bind z))
															 (scale 2.0
															 (cloud-1))))

												(ui:entity
															  ((x -53 ) (z 5))
															  (offset ((bind x) 10 (bind z))
																		 (rotate (0 13 0)
																					(scale 3.0
																					(cloud-1))

																					)))
																								(ui:entity
															  ((x 15 ) (z -23))
															  (offset ((bind x) 10 (bind z))
																		 (rotate (0 -13 0)
																					(scale 4.3
																							 (cloud-1)))))
																								

									
																								
									 ;; draw blended stuff here
									(blend t
									 (offset (0 2 0)
												(rgb (1 1 1 0.3)
													  (scale (200 1 200)
																(tile-model-2)
																)))))

												)
									 

									 )
						  

						  )
				
				)
		 (game:ui)
		 )
						  
						  )))))
