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
  (unless paused
  (with-clist (ui:entity-data ui:entity-id)
	 (let ((radius (car model))
			 (phase (+ phase-offset real-time)))

		(set! x (+ x (* 0.025 (sin phase) radius)))
		(set! z (+ z (* 0.025 (cos phase) radius)))
		(set! angle (* -25.0 real-time))
		)

  )))

(defun move-circle2 (scope model)
  (unless paused
  (with-clist (ui:entity-data ui:entity-id)
	 (let ((radius (car model))
			 (speed (cadr model))
			 (phase phase-offset)
			 (v-x (* 0.025 (sin phase) radius))
			 (v-z (* 0.025 (cos phase) radius)))
		
		(set! x (+ x (* 1.0 v-x) (math:random -0.002 0.002)))
		(set! z (+ z (* 1.0 v-z) (math:random -0.002 0.002)))
		(set! angle
				(+ 180 (/ (math:atan (- v-x) v-z) dec-to-rad))
				)
		(incf phase-offset speed)
		)
  )))


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
(defvar butterfly-wing-1
  '(bake2 (rotate (15 0 0)
	 (offset (1.5 0 0)
	  (scale (1.5 0.1 0.9)
		
		(upcube)
		

		)
	  (offset (0 0 -0.5)
	  (rotate (0 -35 0)
	  (scale (2 0.1 0.9)
		
		(upcube)
		

		)
	  ))))))

(defvar butterfly-1
  '(scale 0.4
	 (rgb (0.3 0.3 0.5)
	 
	 ;; body
	 (scale (0.2 1 0.7)
	  (sphere1))

	 ;;wings
	  (rgb (0.7 0.1 1)
	 (offset (-0.5 0 0)
	  (rotate (0 0 (bind (+ 20 (* 40 (sin (* 5.0 real-time))))))
		(butterfly-wing-1)))
	 
		(offset (0.5 0 0)
	  (rotate (0 0 (bind (+ -20 (* 40 (sin (* -5.0 real-time))))))
	
		(scale (-1 1 1)
				 (butterfly-wing-1))))
		))))

(defvar plane-wing-1
  '(rotate (0 -5 0)
	 (offset (0.5 0 0.2)
	  
	  (rotate (0 5 0)
		(scale (1.0 0.1 3.0)
		 (rgb (0.8 0.2 0.2)
		 (upcube))))
	  (rgb (0.7 0.7 1)

		(offset (0.1 0 -2)
		 (rotate (0 5 0)
		  (for i (range 8)
			(offset (0 0 (bind (- i)))
					  (scale (0.5 0.1 0.5)
								(upcube)))))))


	  )
	  
	 (offset (0.5 0 0)
	  (scale (4.0 0.1 2.0)
		(upcube))
	  (offset (2 0 0)
		(scale (1.0 0.1 1.0)
		 (sphere2)))

  )))

(defvar plane-tail-1
  '(rotate (0 -30 0)
	 (offset (0.5 0 0)
	  (scale (1.2 0.1 1.0)
		(upcube))
	  (offset (0.2 0 0)
		(scale (1.0 0.1 0.5)
		 (sphere2)
		 ))

  )))

(defvar plane-1
  '(bake2 (scale 1.0

			  (rgb (0 0 0)
				(offset (-0.2 0.1 3)
				 (scale (0.4 1.0 1.0)
				  (sphere2))))

			  (rgb (1 0 0)
				(for i (-2 -2.5 -1.5 -1.0)
				(offset (-0.0 0.1 (bind i))
				 (scale (1.0 1.0 0.2)
				  (sphere2)))))
			  
	 (rgb (1.0 0.7 0.3)
	 ;; body
	 (scale (1 1 5.0)
	  (sphere2))

	 ;;wings

	  (offset (1.5 0.5 0.5)
		(plane-wing-1)
	  )
	 
	 (offset (-1.5 0.5 0.5)
	  
		(scale (-1 1 1)
		 (plane-wing-1)))

	  ;; tail
	  (offset (0.2 0 -3.2)
		(plane-tail-1))
	  (offset (-0.2 0 -3.2)
		(scale (-1 1 1)
		 (plane-tail-1)))
	  
	 ))))

(defvar rocket-1
  '(bake0 (scale 1.0
			  (bake2 
			  
			  (rgb (0.6 0.6 0.6)
				;; body
				(scale (1 1 5.0)
				 (sphere2))
				
				;; tail
				(offset (0.2 0 -3.2)
				 (plane-tail-1))
				(offset (-0.2 0 -3.2)
				 (scale (-1 1 1)
				  (plane-tail-1)))))
				
				(offset (0 -0.5 -4.3)
				 
				 (rgb (1 1 0.3)
				  (scale ((bind (math:random 0.7 0.8)) 0.75 2.0)
					(sphere2)))
				 (rgb (1 0.5 0.3)
				  (scale ((bind (+ (math:random -0.1 0.1) 0.6)) 0.6 (bind (+ (math:random 2.4 2.7) 0.6)))
					(sphere2)))

				 )
	  
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
  
  (hashtable-maybe-set object-body ui:entity-id
							  (let ((r (unbind-rec model scope)))
								 (when (not (eq r model))
									(println r '<<new))
								 r))
  )

(defvar butterfly-entity
  '(ui:entity ((x (math:random -20.0 20.0))
					  (y y0)
					  (angle 0)
					  (z (math:random -20.0 20.0))
					  (phase-offset (math:random 0.0 3.0))
					  (hidden nil))
													 ;(body (takeable ui:entity-id)))
		(scope:if (not hidden)
					 ;(move-circle2 5.0 0.05)
					 (ui:body (sphere 1.0))
					 (offset ((bind x) (bind y) (bind z))
													 ;(bake2
								(rgb (0.4 0.8 0.4)
									  (rotate (0 (bind angle) 0)
												 (butterfly-1))))))
  )

(defvar plane-entity
  '(ui:entity ((x (math:random -50.0 50.0))
					(y y0)
					(angle 0)
					(z (math:random -50.0 50.0))
					(phase-offset (math:random 0.0 180.0))
					(hidden nil))
													 
	 (scope:if (not hidden)
				  (move-circle2 15.0 0.01)
				  (ui:body (sphere 3.0))
				  
				  (offset ((bind x) (bind y) (bind z))
							 (rotate (0 (bind (+ 180 angle)) 0)
										(plane-1)))))
  )


(defvar rocket-entity
  '(ui:entity ((x (math:random -50.0 50.0))
					(y y0)
					(angle 0)
					(z (math:random -50.0 50.0))
					(phase-offset (math:random 0.0 180.0))
					(hidden nil))
													 
	 (scope:if (not hidden)
				  (move-circle2 15.0 0.01)
				  (ui:body (sphere 3.0))
				  
				  (offset ((bind x) (bind y) (bind z))
							 (rotate (0 (bind (+ 180 angle)) 0)
										(rocket-1)))))
  )


(defvar bee-entity
  '(ui:entity ((x (math:random -30 30)) (y y0)
					(z (math:random -30 30))
					(angle 0)
					(phase-offset (math:random 0 180))
					(hidden nil)
					(danger t)
					)
	 (move-circle 10.0)
	 (ui:body (sphere 1.0))
	 (offset ((bind x) (bind y) (bind z))
	  (rotate (0 (bind angle) 0)
		(scale 2.0
		 (bee-1)))
	 )))


(defvar ground
  '(vars ((y0 0))
	 (rgb (0.2 0.9 0.2)
	 (for id (range 3)
	  (bee-entity))

	 (for id (range 5)
	  
	  (ui:entity ((x (math:random -20.0 20.0))
					  (y 0.0)
					  (angle 0)
					  (z (math:random -20.0 20.0))
					  (phase-offset (math:random 0.0 3.0))
					  (hidden nil))
													 ;(body (takeable ui:entity-id)))
		(scope:if (not hidden)
					 (move-circle2 5.0 0.05)
					 (ui:body (sphere 1.0))
					 (offset ((bind x) 1.0 (bind z))
													 ;(bake2
								(rgb (0.4 0.8 0.4)
									  (rotate (0 (bind angle) 0)
												 (butterfly-1))));)
	  
					 )))


	 )))

(defvar ground-baked
  '(bake2 (ground)))

(defvar letter-model
  '(rgb (0.9 0.8 0.2)
	 (tile-model-2)))

(defvar space-down-event '(key-down key 32))
(defvar free-fly t)
(defvar paused nil)
(defun game-update (events)
  (let ((player-stats (ui:entity-data :player))
		  (space-down (key-down? foxgl:key-space))
		  )
	 (unless paused
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
			 (let ((speed (+ 0.2 (/ (cdr y) 500.0)))
					 (absv (math:sqrt (+ (* v-z v-z) (* v-x v-x)))))
				(when (> absv 0)
				  (when (eq animation :walk)
					 (incf animation-time 0.04)
					 (set-cdr! (clist-get player-stats 'animation-time) animation-time)
		  			 )
				  
				  (let ((target-angle (/ (math:atan (- v-x) v-z) dec-to-rad)))
					 (set-cdr! angle (interpolate-angle (cdr angle) target-angle 10))					 (set! v-x (* speed (/ v-x absv)))
					 (set! v-z (* speed (/ v-z absv)))
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
												  
												  (set! energy (max 0 (+ energy (if danger -10 1.0))))
										
											 )
										  ))
										))))
				)
			 )
		  (release-matrix mat1)
		  (release-matrix mat2)
		  
		  )

		))))

(defvar cloud-1
  '(bake2 (rgb (0.9 0.9 1)
	 (sphere2)
	 (offset (0.7 0 0.1)
	  (scale 1.2
	  (sphere2))
	  (offset (1.5 0 -0.1)
		(scale 1.5
				 (sphere2)))))))
(defvar danger nil)
(defvar skylevel-scaling 1.0)
(defvar skylevel
  '((bake2 :key (bind y0)
	  (for id (range 0)
		(offset ((bind (math:random -30.0 30.0))
					(bind (+ y0 (math:random -3.0 3.0)))
					(bind (math:random -30.0 30.0)))
		 (scale (bind (math:random 2.0 4.0))
		  (cloud-1)
		))))

	 (scope:if (< player-y (+ y0 25.0))
	
				  (for id (range 5)
						 (butterfly-entity)
						 )
				  (for id (range 3)
						 (bee-entity)

						 ))))

(defvar skylevel-gen
  '(ui:entity  ((y0 (car args)))
	 (scope:if (> player-y (- y0 5.0))
				  (skylevel))
	 ))

(defvar highsky-level

  '(scope:if (< player-y (+ y0 40.0))
	 (for id (range 5)
	  (plane-entity)
	  ))
	 )

(defvar highsky-level-gen
  '(ui:entity ((y0 (car args)))
	 (scope:if (> player-y (- y0 10.0))
				  (highsky-level))))	 


(defvar space-level

  '(ui:entity ((y0 (car args)))
	 (scope:if (< player-y (+ y0 40.0))
				  (for id (range 10)
						 (rocket-entity)
						 )

	  (for id (range 2)

		(ui:entity ((x (math:random -100.0 100.0))
						(y y0)
						(angle 0)
						(z (math:random -100.0 100.0))
						(size (math:random 7.0 10.0))
						(hidden nil)
						(danger t)

						)
		 (ui:body (sphere (bind size)))
					  
		 (scope:if (not hidden)
					  			
									  (offset ((bind x)
												  (bind y0)
												  (bind z))
												 
												 (bake2 :key (bind y0)
														  (rgb ((bind (math:random 0.1 0.4)) (bind (math:random 0.1 0.4)) (bind (math:random 0.1 0.4)) )
												 (scale (bind size)
														  (sphere2))
												 ))
		 )))))
	 (bake2 :key (bind y0)
	  (for id (range 20)
		(rgb ((bind (math:random 0.8 1.0))
				(bind (math:random 0.8 1.0))
				(bind (math:random 0.8 1.0)))
		 (offset ((bind (math:random -100.0 100.0))
					 (bind y0)
					 (bind (math:random -100.0 100.0)))
		  (tile-model-2)
		  (rotate (0 45 0)
			(tile-model-2)
		  )))
		))
	  
	  )
	 
	 )




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
		(vars ((player-y (or (get-data :player 'y) 100)))
		(rgb (1 0 0)
			  (rotate (90 180 0)
						 (ui:entity-root
						  (offset (0 (bind (- -30 (/ player-y 3))) 0)
									 
									 (offset ((bind (- (or (get-data :player 'x) 0)))
												 (bind (- (or (get-data :player 'y) 0)))
												 (bind (- (or (get-data :player 'z) 0))))
												(rgb (0.2 0.9 0.2)
													  (offset (0 -2 0)
																 (scale (100 0.1 100)
																		  (sphere2))))
												
												(ui:entity :ground ((x 0))
															  (scope:if (< player-y 20)
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

												(vars ((y0 5))
														
														)

												(for i (range 10.0 10.0 60.0)
													  (skylevel-gen (bind i)))

												(for i (70.0 90 120 140)
													  (highsky-level-gen (bind i)))

												(for i (140 170 200 230)
													  (space-level (bind i)))

												;; blend step
												
												(for i (range 5.0 5.0 60.0)
									 ;; draw blended stuff here
									(blend t
									 (offset (0 (bind (- player-y 6 i)) 0)
												(rgb (0.8 0.8 0.8 0.2)
													  (scale (100 0.2  100)
																(sphere2)
																)))))

												)
									 

									 )
						  

						  )
				
				)
		 (game:ui)
		 )
						  
						  )))))
