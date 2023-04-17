(load "models-2.lisp")
(load "table.lisp")
(defun gen-id ()
    (incf table-id-iterator 1)
  )
(defvar player-wizard
  '(scale 1.0
	 (offset (0 0 0)
	  (vars ((run-cycle (* 2 wizard-walk)))
		(wizard-model))
	  (offset (-2 0 0)
		(cat-model)
		) 
	  )))

(defvar camera-offset '(0 16 20))
(defvar wizard-offset '(3.0 0.0 5.0))
(defvar wizard-walk 0.0)
(defvar wizard-angle 0.0)

(defvar entities (table-new 'entities '((entity 0 :entity) (x 0.0) (y 0.0) (z 0.0) (angle 0.0))))
(defvar velocities (table-new 'entity-velocity '((entity 0 :entity) (vx 0.0) (vy 0.0) (vz 0.0) (vangle 0.0))))
(defvar entity-model (table-new 'models '((entity 0 :entity) (model 'nothing))))
(defvar active-entities (table-new 'active-entities '((entity 0 :entity))))
(defvar entity-model (table-new 'entity-model '((entity 0 :entity) (model ()))))

(defvar collisions (table-new 'collisions '((entity-a 0 :entity) (entity-b 0 :entity) (collision-data ()))))

(defvar physical-body (table-new 'physical-body
											'((entity 0 :entity)
											  (offset-x 0.0)
											  (offset-y 0.0)
											  (offset-z 0.0)
											  (body ()))))
(defvar dec-to-rad (* pi (/ 2.0 360.0))) 
(defvar wizard-id (gen-id))
(defvar a-id (gen-id))
(defvar b-id (gen-id))
(defvar c-id (gen-id))
(table-insert entities wizard-id 3.0 0.0 5.0 0.0) 
(table-insert entities a-id 10.0 0.0 15.0 0.0) 
(table-insert entities b-id 15.0 0.0 5.0 20.0)
(table-insert entities c-id 13.0 0.0 15.0 0.0)

(table-insert entity-model wizard-id 'player-wizard) 
(table-insert entity-model a-id 'evil-wizard-model)
(table-insert entity-model c-id 'evil-wizard-model)
(table-insert entity-model b-id 'duck-toy)

(table-insert active-entities wizard-id)
(table-insert active-entities a-id)
(table-insert active-entities b-id)
(table-insert active-entities c-id)

(table-insert physical-body wizard-id 0.0 0.0 0.0 '(aabb 1 1 0.7))
(table-insert physical-body a-id 0.0 0.0 0.0 '(aabb 1 1 0.7))
(table-insert physical-body b-id 0.0 0.0 0.0 '(aabb 1 1 0.7))
(table-insert physical-body c-id 0.0 0.0 0.0 '(aabb 1 1 0.7))


(defvar run-cycle 0.0)
(defun interpolate-angle(now target turn-speed)
  (let ((now (mod now 360.0))
		  (target (mod target 360.0)))
	 (let ((diff (- target now)))
		(when (> (abs diff) 180)
		  (set! target (+ target (if (> target now) -360.0 360.0)))

		  (set! diff (- target now))
		  )
		(mod (+ now (* (sign diff) (min turn-speed (abs diff)))) 360))))

(defun game-update (events)
  (let ((v-x 0.0)
		  (v-y 0.0)
		  (v-z 0.0)
		  (v-angle 0.0))
	 (when (key-down? foxgl:key-w)
		(set! v-angle 180)
		(set! v-z -0.1))
	 (when (key-down? foxgl:key-a)
		(set! v-angle 90)
		(set! v-x -0.1))
	 (when (key-down? foxgl:key-d)
		(set! v-angle -90)
		(set! v-x 0.1))
	 (when (key-down? foxgl:key-s)
		(set! v-angle 0)
		(set! v-z 0.1))
	 ;(println v-x v-z)
	 (let ((absv (math:sqrt (+ (* v-z v-z) (* v-x v-x)))))
		(when (> absv 0)
		  (let ((target-angle (/ (math:atan (- v-x) v-z) dec-to-rad)))
		  (set! wizard-walk (+ wizard-walk 0.1))
		  (set! wizard-angle (interpolate-angle wizard-angle target-angle 10))
		  (set! v-x (* 0.1 (/ v-x absv)))
		  (set! v-z (* 0.1 (/ v-z absv)))
		  )
		(table:iter entities :edit
				 	 (when (eq entity wizard-id)
						
						(set! x (+ x v-x))
						(set! y (+ y v-y))
						(set! z (+ z v-z))
						(set! angle (rational wizard-angle)))
					 )
	 

		)
	 ))
  (when t
	 
	 ;; physical body collisions
	 (table:clear collisions)
	 (table:iter (entities physical-body)
					 (let ((e0 entity)
							 (offset-x0 (+ x offset-x))
							 (offset-y0 (+ y offset-y))
							 (offset-z0 (+ z offset-z))
							 (mat-0 (get-matrix))
							 (col-out (cons nil nil))
							 (body-0 body)
												  
							 (angle-0 angle))
						(math:translate! mat-0 offset-x0 offset-y0 offset-z0)
						(math:rotate! mat-0 0 angle-0 0)
						
						(table:iter (entities physical-body)
										(unless (>= entity e0)
										  (let ((mat-1 (get-matrix)))
							
											 
											 (math:translate! mat-1
																	(- offset-x0 (+ x offset-x))
																	(- (+ y offset-y) offset-y0)
																	(- offset-z0 (+ z offset-z)))
											 (math:rotate! mat-1 0 (* dec-to-rad (- angle-0  angle)) 0)
											 
											 (let ((col (sdf:detect-collision body-0 body mat-1 col-out)))
												
												(when col
												  ;(println (list e0 entity col-out))
												  (table-insert collisions e0 entity col-out)
												  )
											 

											 (release-matrix mat-1)
											 ))))
						
						(release-matrix mat-0)
									
						)))
  (when (> (table-rows collisions) 0)
	 (println collisions)
  )
	 
  (when events
	 )
  )

(set! model
  '((perspective (1.0 1.0 0.1 1000.0)
	  (depth t
		(rgb
		 (1 0 0)
		 (rotate
		  (40 0 0)
  		  (offset
			((bind (- (+ (car wizard-offset)
							 (car camera-offset))))
			 (bind (- (+ (cadr wizard-offset)
							 (cadr camera-offset))))
			 (bind (- (+ (caddr wizard-offset)
							 (caddr camera-offset)))))
			
			(offset (-500 0 -500)
					  (rgb (0.35 0.7 0.25)
							 (scale (1000 1 1000)
									  (tile-model))))
			
			(offset (0 -5 -500)
					  (rgb (0.7 0.7 1)
							 (scale (1000 100 100)
									  (upcube))))
			
			(offset (-500 50 -500)
					  (rgb (0.5 0.5 1)
							 (scale (1000 1 1000)
									  (tile-model))))
			
			(for-table (entities active-entities entity-model)
						  (offset ((bind x) (bind y) (bind z))
									 (rotate (0 (bind angle) 0)
												(scale 0.4
														 (offset (0 0 0)
																	(bind2 model))
																	))))
			
			
			(scale 1.0
					 (rotate (0.0 0 0.0)
								(for i (range -30 30 30)
				 					  (for j (range -30 30 30)
											 (offset ((bind i) 0 (bind j))
														(rotate (0 (bind (* 0 (+ i j))) 0) 
																  (cherry-tree 6 (bind (+ i (* j 100))))))))
								(offset (0 0 0)
										  (vars ((run-cycle (* 2 real-time)))
												  (rotate (0 (bind (* 10.0 run-cycle)) 0)
															 (offset (4 0 0)
   																	(horse))))
										  (vars ((run-cycle (* 2 real-time)))
												  (rotate (0 2 0)
															 (offset (8 0 0)
																		(scale (0.3 0.3 0.3)
																				 (rotate (0 (bind (* 3 real-time)) 0)
																							(elephant))))))
										  (vars ((run-cycle (* 2 real-time)))
												  (rotate (0 4 0)
															 (offset (12 0 0)
																		(scale (0.3 0.3 0.3)
																				 (rotate (0 (bind (* 5 real-time)) 0)
   																						(duck-toy))))))
										  
										  
										  )
								)))))
		))))

(defvar model2
  '(perspective (1.0 1.0 0.1 1000.0)
	 (depth t
	  (rgb (1 0 0)
		(offset (0 0 -5)
				  (rotate (0 (bind real-time) 0)
							 (wizard-model)))))))

