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
 	 (let ((data (hashtable-ref entity-data ui:entity-id))) 
		
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
(defvar wing-flap 0.0)
(defvar wing-in -80.0)
(defvar tail-spread 5)
(defvar bird-wing
  '(rgb (1 1 1)
	 (rotate (0 (bind wing-in) (bind (* 22 (sin (* 2.0 pi wing-flap)))))
	  (offset (-0.25 0 0)
		(scale (1.5 1.0 1.0)
				 (right-tile))
	  (offset (1.4 0 0)
		(rotate (0 0 (bind (* 22 (sin (* 2.0 pi wing-flap)))))
				  (offset (-0.25 0 0)
							 (scale (1.5 1.0 1.0)
									  (right-tile))
	 
	  )))))))

(defvar bird-model
  '(offset (0 0 1)
	 (rgb (1 1 1)
	 ;;head
	 (tile-model-2)
	 ;; eyes
	 (rgb (0 0 0)
	  (offset (0.2 0.1 0.1)
		(scale (0.1 1.0 0.2)
				 (tile-model-2)))
	  (offset (-0.2 0.1 0.1)
		(scale (0.1 1.0 0.2)
				 (tile-model-2)))

	  )

	 
	 ;; beak
	 (offset (0 0 1)
	  (rgb (1 1 0)
		(rotate (0 0 0)
				  (scale (0.5 1.0 1.0)
							(tile-model-2)))))
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
	 )))
(defvar ground
  '(rgb (0 1 0)
	 (scale (200 0 200)
	  (tile-model-2))
	 (for id (range 400)
	  (ui:entity ((x (math:random -100.0 100.0))
					  (z (math:random -100.0 100.0)))
		(offset ((bind x) 1.0 (bind z))
				  (bake2
				  (rgb (1 0 0)
						 (tile-model-2))))
	  
		))))

(defvar ground-baked
  '(bake2 (ground)))
(defvar letter-model
  '(rgb (0.9 0.8 0.2)
	 (tile-model-2)))

(defvar space-down-event '(key-down key 32))

(defun game-update (events)
  
  (let ((player-stats (ui:entity-data :player))
		  (space-clicked nil)
		  )
	 (for-each event events
				  (when (equals? event space-down-event)
					 (set! space-clicked t)))
	 (when player-stats
		(let ((z (clist-get player-stats 'z))
				(y (clist-get player-stats 'y))
				(x (clist-get player-stats 'x))
				(angle (clist-get player-stats 'angle))
				(animation-time (cdr (clist-get player-stats 'animation-time)))
				(animation (cdr (clist-get player-stats 'animation)))
				)
		  (println y)
		  (when (eq animation :fly)
			 (when (> animation-time 0.0)
				(set-cdr! y (+ (cdr y) 0.04))
			 
				(incf animation-time 0.04))
			 (when (> animation-time 2.0)
				(set! animation-time 0.0))
			 )
		  (when space-clicked
			 (println 'space!)
			 (set! animation-time (+ 0.01 animation-time)))
		  ;(set-cdr! z (+ 0.01 (cdr z)))
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
													 ;(println v-x v-z)
			 (let ((absv (math:sqrt (+ (* v-z v-z) (* v-x v-x)))))
				(when (> absv 0)
				  (let ((target-angle (/ (math:atan (- v-x) v-z) dec-to-rad)))
					 ;(set! wizard-walk (+ wizard-walk 0.2))
					 (set-cdr! angle (interpolate-angle (cdr angle) target-angle 10))
					 ;(set-cdr! angle angle)
					 (set! v-x (* 0.2 (/ v-x absv)))
					 (set! v-z (* 0.2 (/ v-z absv)))
					 (set-cdr! x (+ v-x (cdr x)))
					 (set-cdr! z (+ v-z (cdr z)))
					 )
				  ;(table-insert velocities wizard-id v-x 0.0 v-z 0.0)
				  )
				))


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

(defun get-data (id sym)
  (cdr (clist-get (ui:entity-data id) sym))
  )

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
														  (ground-baked))
												
												
											 
									
									(ui:entity :player ((x 0) (y 0) (z 0)
															  (angle 0)
															  (animation :fly)
															  (animation-time 0.0))
												  (vars ((wing-flap animation-time)
															(wing-in (if (eq animation :walk) -80 0))
															(tail-spread (if (eq animation :walk) 3 8))													)
												  
										(offset ((bind (println x)) (bind y) (bind z))
												  (rotate (0 (bind angle) 0)
												  (bird-model))))

												  )
									(ui:entity :letter ((x 10) (y 0) (z -5))
												  (offset ((bind x) (bind y) (bind z))
															 (letter-model)))

												
												
												(scope:if (> player-y 5.0)
															 (ui:entity 123213
															  ((x 0 ) (z 0))
												  (offset ((bind x) 5 (bind z))
															 (cloud-1)))

												(ui:entity 1232133
															  ((x 5 ) (z 5))
															  (offset ((bind x) 10 (bind z))
																		 (rotate (0 13 0)
															 (cloud-1))))

									(blend t
									 (offset (0 2 0)
												(rgb (1 1 1 0.4)
													  (scale (200 1 200)
																(tile-model-2)
																)))))
						 

									))
						  )
						  
						  )))))))
