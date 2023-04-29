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
	  (offset (-0.6 2 0)
		(scale 0.4
				 (cat-model))
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

(defvar bullets (table-new 'bullets '((entity 0 :entity) (vx 0.0) (vz 0.0) (life 0.0))))

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

(table-insert physical-body wizard-id 0.0 0.0 0.0 '(sphere 1.0))
(table-insert physical-body a-id 0.0 0.0 0.0 '(aabb 1 1 0.7))
(table-insert physical-body b-id 0.0 0.0 0.0 '(aabb 1 1 1.2))
(table-insert physical-body c-id 0.0 0.0 0.0 '(aabb 1 1 0.7))

(defvar house-1-id (gen-id))
(table-insert entities house-1-id -10.0 0.0 0.0 0.0) 
(table-insert entity-model house-1-id 'house-1-model)
(table-insert active-entities house-1-id)
(table-insert physical-body house-1-id 0.0 0.0 0.0 '(aabb 2 1.5 2))


(defvar house-1-id (gen-id))
(table-insert entities house-1-id -20.0 0.0 7.0 -45.0) 
(table-insert entity-model house-1-id 'house-1-model)
(table-insert active-entities house-1-id)
(table-insert physical-body house-1-id 0.0 0.0 0.0 '(aabb 2 1.5 2))


(defvar cherry-tree-1-6-model '(cherry-tree 6 1))
(defvar cherry-tree-2-6-model '(cherry-tree 6 2))

(defvar tree-1-id (gen-id))
(table-insert entities tree-1-id -6.5 0.0 0.0 5.2) 
(table-insert entity-model tree-1-id 'cherry-tree-1-6-model)
(table-insert active-entities tree-1-id)
(table-insert physical-body tree-1-id 0.0 0.0 0.0 '(aabb 0.5 5.0 0.5))

(defvar tree-1-id (gen-id))
(table-insert entities tree-1-id 10.0 0.0 0.0 -3.0) 
(table-insert entity-model tree-1-id 'cherry-tree-2-6-model)
(table-insert active-entities tree-1-id)
(table-insert physical-body tree-1-id 0.0 0.0 0.0 '(aabb 0.5 5.0 0.5))

(defvar tree-1-id (gen-id))
(table-insert entities tree-1-id 5.0 0.0 -15.0 90.0) 
(table-insert entity-model tree-1-id 'cherry-tree-2-6-model)
(table-insert active-entities tree-1-id)
(table-insert physical-body tree-1-id 0.0 0.0 0.0 '(aabb 0.5 5.0 0.5))


(defvar rock-1-id (gen-id))
(table-insert entities rock-1-id 20.0 0.0 -15.0 90.0) 
(table-insert entity-model rock-1-id 'rock-model-1)
(table-insert active-entities rock-1-id)
(table-insert physical-body rock-1-id 0.0 0.0 0.0 '(aabb 2.5 5.0 2.5))

(defvar rock-1-id (gen-id))
(table-insert entities rock-1-id 5.0 0.0 -20.0 0.0) 
(table-insert entity-model rock-1-id 'rock-model-1)
(table-insert active-entities rock-1-id)
(table-insert physical-body rock-1-id 0.0 0.0 0.0 '(aabb 2.5 5.0 2.5))


(defvar cat-2-model '(scale 1.0 (cat-model)))
(defvar cat-1-id (gen-id))
(table-insert entities cat-1-id 0.0 0.0 5.0 .0) 
(table-insert entity-model cat-1-id 'cat-2-model)
(table-insert active-entities cat-1-id)
(table-insert physical-body cat-1-id 0.0 0.0 0.0 '(aabb 0.5 0.5 0.5))

(defvar bullet-2-model '(rgb (1 1 1) (scale 0.5 (upsphere))))
(defvar bullet-1-id (gen-id))
(table-insert entities bullet-1-id 0.0 1.0 10.0 0.0) 
(table-insert entity-model bullet-1-id 'bullet-2-model)
(table-insert active-entities bullet-1-id)
;(table-insert physical-body bullet-1-id 0.0 0.0 0.0 '(aabb 0.5 0.5 0.5))
(table-insert bullets bullet-1-id 0.1 0.0 50.0)



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
  (table:clear velocities)
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
		  (set! wizard-walk (+ wizard-walk 0.2))
		  (set! wizard-angle (interpolate-angle wizard-angle target-angle 10))
		  (set! v-x (* 0.2 (/ v-x absv)))
		  (set! v-z (* 0.2 (/ v-z absv)))
			 )
		  (table-insert velocities wizard-id v-x 0.0 v-z 0.0)
		)
	 ))
  (when t
	 
	 ;; physical body collisions
	 (table:clear collisions)
	 (table:iter (entities physical-body velocities)
					 (let ((e0 entity)
							 (offset-x0 (+ x offset-x vx))
							 (offset-y0 (+ y offset-y vy))
							 (offset-z0 (+ z offset-z vz))
							 (mat-0 (get-matrix))
							 (body-0 body)
												  
							 (angle-0 angle))
						(math:rotate! mat-0 0 (* dec-to-rad angle-0) 0)
						(math:translate! mat-0 offset-x0 offset-y0 offset-z0)
						
						(table:iter (entities physical-body)
										(unless (= entity e0)
										  (let ((mat-1 (get-matrix))
												  (col-out (make-vector 5 0.0))
												  )
											 (math:rotate! mat-1 0 (* dec-to-rad angle) 0)
	
											 
											 (math:translate! mat-1
																	(+ x offset-x)
																	(+ y offset-y)
																	(+ z offset-z))
																	 
											 (let ((col (sdf:detect-collision2 body-0 body mat-0 mat-1 col-out)))
												
												(when col
													 ;(println (list e0 entity col-out))
												  (table-insert collisions e0 entity col-out)
	
												  )
											 

											 (release-matrix mat-1)
											 ))))
						
						(release-matrix mat-0)
									
						)))
  (when (> (table-rows collisions) 0)
	 (println (list velocities collisions))
	 (table:iter (velocities collisions entities) :edit
					 ;(set! vx (* vx -0.01));0.0)
					 ;(set! vz (* vz -0.01));0.0)
					 (let* ((dx (- (vector-ref collision-data 0) x))
							  (dz (- (vector-ref collision-data 2) z))
							  (dl (math:sqrt (+ (* dx dx) (* dz dz)))))
						(set! dx (/ dx dl))
						(set! dz (/ dz dl))
						(let ((cor-x (* (vector-ref collision-data 3) dx))
								(cor-z (* (vector-ref collision-data 3) dz)))
						  (set! vx (+ vx (* 0.9 cor-x)))
						  (set! vz (+ vy (* 0.9 cor-z)))
						
						
						))
								  
					 ;(if (< (car collision-data) (cdr collision-data))
					;	  (set! vx (- vx (* 1.0 (car collision-data))))
					;	  (set! vz (- vz (* 1.0 (cdr collision-data))))
													 ;	  )
					 ;(let ((cx (car collision-data))
					;		 (cz (cdr collision-data)))
													 ;	(println (list vx vz collision-data (cons (- x cx) (- z cz)))))
					 (println collision-data)
						  
					 )
	 (println collisions)
	 )
  (let ((clear-bullets nil))
  (table:iter (entities bullets) :edit
				  (set! x (+ x vx))
				  (set! z (+ z vz))
				  (set! life (- life 1.0))
				  (when (> 0 life)
					 (println life)
					 (set! clear-bullets (cons entity clear-bullets))
					 
					 (println 'remove)
					 )
				  
				  )
  (when clear-bullets
		(loop clear-bullets
				(table:iter (bullets entities) :edit 
								(when (eq (car clear-bullets) entity)
								  :remove))
				(set! clear-bullets (cdr clear-bullets))

				)
		
	 ))
	
  (table:iter (velocities entities) :edit
				  (set! x (+ x vx))
				  (set! y (+ y vy))
				  (set! z (+ z vz))
					 
				  (when (eq entity wizard-id)
					 
					 (set! wizard-offset (list x y z))
					 (set! angle (rational wizard-angle)))
					 
				  )
	 
	 
	 (when events
		(for-each event events
					 (when (equals? event '(key-down key 32))
						(println 'space!)
						(let ((bullet-id (gen-id)))
						  (println wizard-offset)
						  (table-insert entities bullet-id (car wizard-offset) 1.0 (caddr wizard-offset) 0.0) 
						  (table-insert entity-model bullet-id 'bullet-2-model)
						  (table-insert active-entities bullet-id)
						  (table-insert bullets bullet-id (cos (* dec-to-rad (+ 90 wizard-angle))) (sin (* dec-to-rad (+ 90 wizard-angle))) 500.0)
						  (println bullets)

						  )
						)
					 )
	 (println events)
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
			(flower-floor-model)
			(for-table (entities active-entities entity-model)
						  (offset ((bind x) (bind y) (bind z))
									 (rotate (0 (bind angle) 0)
												(bind2 model))
												))
			
			
			(scale 1.0
					 (rotate (0.0 0 0.0)
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

