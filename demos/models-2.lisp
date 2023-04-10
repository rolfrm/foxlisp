(defvar leg-rotation-1 0)
(defvar leg-rotation-2 0)
(defvar horse-leg
  '(offset (0 1 0)
	 (rotate ((bind leg-rotation-1) 0 0)
	  (scale (0.1 0.5 0.1)
	  
		(downcube))
	  (offset (0 -0.5 0)
		(rotate ((bind leg-rotation-2) 0 0)
		 (scale (0.1 0.5 0.1)
	 	  (downcube)))
		))))
(defvar run-cycle 0.0)
(defvar horse
  '(rgb (0.7 0.4 0.3)
	 (horsebody
	  (vars ((leg-rotation-1 (* 50 (sin run-cycle)))
				(leg-rotation-2 (+ (* 40 (sin run-cycle)) 40))
				)
      (offset (-0.2 0 -1) (horse-leg))))
	 (vars ((leg-rotation-1 (* 50 (sin (+ 10 run-cycle))))
				(leg-rotation-2 (+ (* 40 (sin (+ 10 run-cycle))) 40))
				)
     (offset (0.2 0 -1) (horse-leg)))
	 (vars ((leg-rotation-1 (* 45 (sin run-cycle)))
				(leg-rotation-2 (+ (* 40 (sin run-cycle)) 40))
				)
     (offset (-0.2 0 1) (horse-leg)))
	 (vars ((leg-rotation-1 (* 45 (sin (+ 10 run-cycle))))
				(leg-rotation-2 (+ (* 40 (sin (+ 10 run-cycle))) 40))
				)

     (offset (0.2 0 1) (horse-leg)))
      (offset (0 1 0)
		 (offset (0 0.9 -0.95)
		  (rotate (0 0 (bind (* -10 (sin run-cycle))))
		  
		  (rotate (-150 0.0 00)
			(scale (0.2 1.0 0.2)
          (rgb (0.8 0.6 0.6)
			  (upcube))))))
        (scale (0.7 1.1 2.0) 
			(upcube))
		 (offset (0 -0.04 0)
        (scale (0.75 1 1.0)
			(rgb (0.8 0.6 0.6)
			 (upcube))))
		 (offset
		  (0 0.8 1)
		  (rotate (0 0 (bind (* 10 (sin run-cycle))))
		  (rotate (30 0 0)
		    (scale (0.5 0.6 1.0)
			(upcube)))
		  (rgb (0 0 0)
			(translate (0.25 0.2 0.30)
			 (rotate (-25 0 0)
			 (scale (0.05 0.25 0.1)
			  (upcube))))
			 (rotate (-25 0 0)
			 (translate (-0.35 0.2 0.20)
			 (scale (0.05 0.25 0.1)
			  (upcube))
			 )))
		  )
		  ))))

(defvar cherry-tree-color '(0.5 0.4 0.4))
(defvar cherry-flower-color1 '(1 0.8 0.8))
(defvar cherry-flower-color2 '(1.0 0.75 0.75))
(defvar cherry-green-color-1 '(0.3 0.7 0.3))
(defvar cherry-green-color-2 '(0.25 0.6 0.25))

(defvar cherry-tree-sub-scale 0.8)
(defvar cherry-sub
  '(sub
	 (scope:if (> it 0)
				  (rotate (0 (random 0 180) 0)
						(scale (bind cherry-tree-sub-scale)
								 (cherry-tree-0 (bind it)))))
	 (scope:if (< it 3)
				  (offset (0 0 -2)
				  (scale 2.1
							(rgb (bind (eval (car args)))
								  (upcube)
								  ))))
	 ))
(defvar cherry-randomize 50.0)
(defvar cherry-tree-0
  '(rgb (bind cherry-tree-color)
	  (vars ((it (- (or (car args) 5) 1))
				(l (random 1.0 5.0))
				)
		(scale (1 (bind l) 1)
		 (skew (0.1 0.1 0.1)
		  (upcube)))
		(offset (0 (bind l) 0)
		 (for x (cherry-flower-color1
					;cherry-green-color cherry-green-color-2
													 ;cherry-flower-color1
					cherry-flower-color2)
		  (rotate ((bind (random cherry-randomize  (- 0 cherry-randomize)))
					  (bind (random cherry-randomize  (- 0 cherry-randomize)))
					  (bind (random cherry-randomize  (- 0 cherry-randomize)))
					  )
		  (cherry-sub (bind x)))
		 )))))

(defvar cherry-tree
  '(bake2 :key (bind (cadr args))
	 (cherry-tree-0 (bind (car args)))
	 ))


(defvar elephant
  '(elephant-model
;; Body
(translate (0 2 0)
           (rgb (0.5 0.5 0.5)
                (scale (4 6 2)
                       (upcube))))

;; Head
(translate (0 4.5 3)
           (rgb (0.5 0.5 0.5)
                (scale (2 2 2)
                       (upcube))))

;; Trunk
(translate (0 3 5)
           (rgb (0.5 0.5 0.5)
                (scale (0.5 2 0.5)
													 ;(cylinder 1 1)
					  )))

;; Legs
(translate (-1.5 -1 0)
           (rgb (0.5 0.5 0.5)
                (scale (0.75 3 0.75)
                       (upcube))))
(translate (1.5 -1 0)
           (rgb (0.5 0.5 0.5)
                (scale (0.75 3 0.75)
                       (upcube))))
(translate (-1.5 -1 -4)
           (rgb (0.5 0.5 0.5)
                (scale (0.75 3 0.75)
                       (upcube))))
(translate (1.5 -1 -4)
           (rgb (0.5 0.5 0.5)
                (scale (0.75 3 0.75)
                       (upcube))))

;; Ears
(translate (-2.5 4.25 1.5)
           (rgb (0.5 0.5 0.5)
                (scale (0.25 1.5 1.5)
                       (upcube))))
(translate (2.5 4.25 1.5)
           (rgb (0.5 0.5 0.5)
                (scale (0.25 1.5 1.5)
                       (upcube))))

;; Tail
(translate (0 0 -6)
           (rgb (0.5 0.5 0.5)
                (scale (0.25 1.5 0.25)
                 ;(cylinder 1 1)
					  )))

;; Left Eye
(translate (-1.25 5 4.5)
           (rgb (0 0 0)
                (scale (0.25 0.25 0.25)
                       (upcube))))

;; Right Eye
(translate (1.25 5 4.5)
           (rgb (0 0 0)
                (scale (0.25 0.25 0.25)
                       (upcube))))
	 ))

(defun generate-cylinder-triangle-strip (segments radius height)
  (let* ((angle-step (/ (* 2 pi) segments))
         (vertices '()))
    ;; Generate side triangle strip
    (dotimes! i segments
      (let* ((angle1 (* i angle-step))
             (angle2 (* (mod (+ 1 i) segments) angle-step))
             (x1 (* radius (cos angle1)))
             (z1 (* radius (sin angle1)))
             (x2 (* radius (cos angle2)))
             (z2 (* radius (sin angle2))))
        (set! vertices (concat vertices (list x1 0 z1 x1 height z1 x2 0 z2)))
        (set! vertices (concat vertices (list x2 0 z2 x1 height z1 x2 height z2)))))
    ;; Add degenerate triangle
    (let* ((angle1 (* segments angle-step))
           (x1 (* radius (cos angle1)))
           (z1 (* radius (sin angle1))))
      (set! vertices (concat vertices (list x1 0 z1 x1 0 z1))))
    ;; Generate top and bottom triangle strip
    (dotimes! i segments
      (let* ((angle1 (* i angle-step))
             (angle2 (* (mod (+ 1 i) segments) angle-step))
             (x1 (* radius (cos angle1)))
             (z1 (* radius (sin angle1)))
             (x2 (* radius (cos angle2)))
             (z2 (* radius (sin angle2))))
        (set! vertices (concat vertices (list x1 0 z1 0 0 0 x2 0 z2)))
        (set! vertices (concat vertices (list x1 height z1 x2 height z2 0 0 height)))))
    vertices))

(defun generate-cone-triangle-strip (segments radius height)
  (let* ((angle-step (/ (* 2 pi) segments))
         (vertices '()))
    ;; Generate side triangle strip
    (dotimes! i segments
      (let* ((angle1 (* i angle-step))
             (angle2 (* (mod (+ 1 i) segments) angle-step))
             (x1 (* radius (cos angle1)))
             (z1 (* radius (sin angle1)))
             (x2 (* radius (cos angle2)))
             (z2 (* radius (sin angle2))))
        (set! vertices (concat vertices (list x1 0 z1 0 height 0 x2 0 z2)))))
    ;; Add degenerate triangle
    (let* ((angle1 (* segments angle-step))
           (x1 (* radius (cos angle1)))
           (z1 (* radius (sin angle1))))
      (set! vertices (concat vertices (list x1 0 z1 x1 0 z1))))
    ;; Generate bottom triangle strip
    (dotimes! i segments
      (let* ((angle1 (* i angle-step))
             (angle2 (* (mod (+ 1 i) segments) angle-step))
             (x1 (* radius (cos angle1)))
             (z1 (* radius (sin angle1)))
             (x2 (* radius (cos angle2)))
             (z2 (* radius (sin angle2))))
        (set! vertices (concat vertices (list x1 0 z1 0 0 0 x2 0 z2)))))
    vertices))

(defun generate-pyramid-triangle-strip (base-side-length height)
  (let* ((half-base (/ base-side-length 2))
         (vertices '()))
    ;; Generate side triangle strip
    (set! vertices (concat vertices (list
                                     ;; Base
                                     (- 0 half-base) 0 half-base
                                     ;; Apex
                                     0 height 0
                                     ;; Base
                                     half-base 0 half-base
                                     ;; Apex
                                     0 height 0
                                     ;; Base
                                     half-base 0 (- 0 half-base)
                                     ;; Apex
                                     0 height 0
                                     ;; Base
                                     (- 0 half-base) 0 (- 0 half-base)
                                     ;; Apex
                                     0 height 0
                                     ;; Base
                                     (- 0 half-base) 0 half-base)))
    ;; Add degenerate triangle
    (set! vertices (concat vertices (list (- 0 half-base) 0 half-base (- 0 half-base) 0 half-base)))
    ;; Generate bottom triangle strip
    (set! vertices (concat vertices (list
                                     ;; Base
                                     (- 0 half-base) 0 half-base
                                     ;; Center
                                     0 0 0
                                     ;; Base
                                     half-base 0 half-base
                                     ;; Center
                                     0 0 0
                                     ;; Base
                                     half-base 0 (- 0 half-base)
                                     ;; Center
                                     0 0 0
                                     ;; Base
                                     (- 0 half-base) 0 (- 0 half-base)
                                     ;; Center
                                     0 0 0
                                     ;; Base
                                     (- 0 half-base) 0 half-base)))
    vertices))


(println 'cylinder (generate-cylinder-triangle-strip 10 1.0 1))

(defvar cube cube-2)
(defvar cone `(polygon :3d-triangle-strip ,(generate-cone-triangle-strip 10 1.0 1.0)))

(defvar cylinder `(polygon :3d-triangle-strip ,(generate-cylinder-triangle-strip 10 1.0 1)))

(defvar pyramid `(polygon :3d-triangle-strip ,(generate-pyramid-triangle-strip 1.0 1.0)))

(defvar elephant
  '(union ;; Elephant model
  ;; Body
  (offset (0 5 0)
    (rgb (0.5 0.5 0.5)
      (scale (6 4 10)
        (cube))))

  ;; Legs
  ;; Front Left Leg
  (offset (-2 2 -4)
    (rgb (0.5 0.5 0.5)
      (scale (1 4 1)
        (cube))))
  
  ;; Front Right Leg
  (offset (2 2 -4)
    (rgb (0.5 0.5 0.5)
      (scale (1 4 1)
        (cube))))

  ;; Rear Left Leg
  (offset (-2 2 4)
    (rgb (0.5 0.5 0.5)
      (scale (1 4 1)
        (cube))))
  
  ;; Rear Right Leg
  (offset (2 2 4)
    (rgb (0.5 0.5 0.5)
      (scale (1 4 1)
        (cube))))

  ;; Head
  (offset (0 7 -9)
    (rgb (0.5 0.5 0.5)
      (scale (4 3 3)
        (cube))))

  ;; Ears
  ;; Left Ear
  (offset (-3.5 7 -8)
    (rgb (0.5 0.5 0.5)
      (rotate (0 0 -45)
        (scale (0.5 3 3)
          (cube)))))

  ;; Right Ear
  (offset (3.5 7 -8)
    (rgb (0.5 0.5 0.5)
      (rotate (0 0 45)
        (scale (0.5 3 3)
          (cube)))))
(offset (0 5 -14)
  (rgb (0.5 0.5 0.5)
    (rotate (0 0 -30)
      (scale (0.7 0.7 6)
        (cube)))))
	 ;; Left Eye
(offset (-1.5 8 -12)
  (rgb (0 0 0)
    (scale (0.5 0.5 0.5)
      (sphere))))

;; Right Eye
(offset (1.5 8 -12)
  (rgb (0 0 0)
    (scale (0.5 0.5 0.5)
     (sphere))))
 ;; Tusks
;; Left Tusk
(offset (-1.2 5 -12)
  (rgb (1 1 1)
    (rotate (0 -30 -20)
      (scale (0.3 0.3 2)
        (cube)))))

;; Right Tusk
(offset (1.2 5 -12)
  (rgb (1 1 1)
    (rotate (0 30 20)
      (scale (0.3 0.3 2)
        (cube)))))
))


(defvar duck-toy
  '(scene
  ;; Body
  (offset (0 2 0)
    (rgb (1 1 0)
      (scale (4 2 6)
        (cube))))
        
  ;; Head
  (offset (0 4 -4)
    (rgb (1 1 0)
      (scale (2 2 2)
        (cube))))
  
  ;; Wings
  ;; Left Wing
  (offset (-2.5 2 0)
    (rgb (1 0.9 0)
      (rotate (0 0 -45)
        (scale (1 0.5 3)
          (cube)))))
          
  ;; Right Wing
  (offset (2.5 2 0)
    (rgb (1 0.9 0)
      (rotate (0 0 45)
        (scale (1 0.5 3)
          (cube)))))
          
  ;; Beak
  (offset (0 4 -6)
    (rgb (1 0.5 0)
      (scale (0.5 0.5 0.5)
       (cube))))

	 ;; Eyes
;; Left Eye
(offset (-0.5 5 -5.5)
  (rgb (0 0 0)
    (scale (0.25 0.25 0.25)
      (cube))))

;; Right Eye
(offset (0.5 5 -5.5)
  (rgb (0 0 0)
    (scale (0.25 0.25 0.25)
      (cube))))
))

(defvar duck-toy2
  '(offset (0 -0.5 0)
	 (rgb (1 1 0)   ; yellow color
     (sphere)
      (offset (0 0.5 0) (scale (0.7 0.3 0.7) (sphere1)))
      (offset (-0.4 0.4 0) (scale (0.3 0.3 0.3) (sphere1)))
      (offset (0.4 0.4 0) (scale (0.3 0.3 0.3) (sphere1)))
      (offset (-0.5 -0.5 0) (rotate (0 0 45) (cylinder)))
      (offset (0.5 -0.5 0) (rotate (0 0 -45) (cylinder)))
		(offset (0 0.3 0.5) (scale (0.4 0.4 0.4) (fastigium)))
		)
	  )
	 )

(defvar rocket-model
  ';; Rocket model
(progn
  ;; Body (cylinder)
  (offset (0 0 0)
    (rgb (0.8 0.1 0.1)
      (scale (1 8 1)
        (cube))))
  
  ;; Nose cone (cone)
  (offset (0 4.5 0)
    (rgb (0.8 0.3 0.3)
      (scale (1 1 1)
        (rotate (90 0 0)
          (scale (1 2 1)
            (cube))))))
  
  ;; Fin 1
  (offset (-0.5 -3.5 0.5)
    (rgb (0.9 0.1 0.1)
      (scale (0.1 2 1)
        (cube))))
  
  ;; Fin 2
  (offset (-0.5 -3.5 -0.5)
    (rgb (0.9 0.1 0.1)
      (scale (0.1 2 1)
        (rotate (0 180 0)
          (cube)))))
  
  ;; Fin 3
  (offset (0.5 -3.5 0.5)
    (rgb (0.9 0.1 0.1)
      (scale (0.1 2 1)
        (rotate (0 0 180)
          (cube)))))
  
  ;; Fin 4
  (offset (0.5 -3.5 -0.5)
    (rgb (0.9 0.1 0.1)
      (scale (0.1 2 1)
        (rotate (0 180 180)
          (cube)))))
  ))


(defvar house-1
  '(union
  ;; House main body
  (offset (0 0.5 0)
    (rgb (0.8 0.4 0.4)
      (scale (4 2 4)
        (cube))))

  ;; Roof (fastigium)
  (offset (0 3.5 0)
    (rgb (0.7 0.3 0.3)
      (scale (4.5 1.5 4.5)
        (pyramid))))

  ;; Door
  (offset (0 0.5 -2)
    (rgb (0.5 0.25 0.25)
      (scale (1 2 0.1)
        (cube))))

  ;; Left window
  (offset (-1 1.5 -2)
    (rgb (0.7 0.7 1)
      (scale (0.7 0.7 0.1)
        (cube))))

  ;; Right window
  (offset (1 1.5 -2)
    (rgb (0.7 0.7 1)
      (scale (0.7 0.7 0.1)
        (cube))))))


(defvar wizard-model
  '(union
  ;; Head (sphere)
  (rgb (0.9 0.8 0.7)
   (scale (1 1 1)
	 (translate (0 1 0)
     (sphere1)
	  )))

    ;; Left eye (sphere)
    (rgb (0 0 0)
      (translate (-0.3 0.9 1.0)
        (scale (0.1 0.1 0.1)
          (sphere1))))

    ;; Right eye (sphere)
    (rgb (0 0 0)
      (translate (0.3 0.9 1.0)
        (scale (0.1 0.1 0.1)
          (sphere1))))
	 
  ;; Body (cylinder)
  (rgb (0.5 0.3 0.8)
    (translate (0 -3 0)
      (scale (1 3 1)
       (cylinder)
		 )))

  ;; Hat (cone)
  (rgb (0.5 0.3 0.8)
    (translate (0 2 0)
      (scale (1 3 1)
       (cone)
		 )))

  ;; Hat brim (fastigium)
  (rgb (0.5 0.3 0.8)
    (translate (0 2 0)
      (scale (3 0.2 3)
        (pyramid))))

  ;; Left arm (cylinder)
  (rgb (0.9 0.8 0.7)
    (translate (-1.5 -2.5 0)
      (rotate 0 0 0.785)
        (scale (0.5 2 0.5)
         (cylinder)
			)))

  ;; Right arm (cylinder)
  (rgb (0.9 0.8 0.7)
    (translate (1.5 -2.5 0)
      (rotate 0 0 -0.785)
        (scale (0.5 2 0.5)
         (cylinder)
			)))

  ;; Left leg (cylinder)
  (rgb (0.5 0.3 0.8)
    (translate (-0.5 -4.5 0)
      (scale (0.5 2 0.5)
       (cylinder)
		 )))

  ;; Right leg (cylinder)
  (rgb (0.5 0.3 0.8)
    (translate (0.5 -4.5 0)
      (scale (0.5 2 0.5)
       (cylinder)
		 )))

  ;; Staff (cylinder)
  (rgb (0.4 0.2 0.1)
    (translate (2.5 -3 0)
      (rotate 0 0 -0.785)
        (scale (0.2 5 0.2)
         (cylinder)
			)))

  ;; Staff top (sphere)
  (rgb (0.7 0 0)
    (translate (2.5 2 0)
      (scale (0.5 0.5 0.5)
       (sphere1))))

	 ;; Left foot (cube)
(rgb (0.5 0.3 0.8)
  (translate (-0.5 -4.5 0.5)
    (scale (0.5 0.5 1)
      (cube))))

;; Right foot (cube)
(rgb (0.5 0.3 0.8)
  (translate (0.5 -4.5 0.5)
    (scale (0.5 0.5 1)
      (cube))))
))


(defvar cat-model
  '(union
    ;; Body (ellipsoid)
    (rgb (0.3 0.3 0.3)
      (translate (0 0.6 0)
        (scale (1 1.3 1)
         (sphere1)
			)))

    ;; Head (sphere)
    (rgb (0.3 0.3 0.3)
      (translate (0 2 0)
        (scale (0.6 0.6 0.6)
			(sphere1)
			)))

    ;; Left ear (pyramid)
    (rgb (0.3 0.5 0.3)
      (translate (-0.3 2.3 0)
        (rotate 0 (-0.785) 0)
          (scale (0.2 0.4 0.2)
            (pyramid))))

    ;; Right ear (pyramid)
    (rgb (0.5 0.3 0.3)
      (translate (0.3 2.3 0)
        (rotate 0 0.785 0)
          (scale (0.2 0.4 0.2)
            (pyramid))))

    ;; Left eye (sphere)
    (rgb (1 1 1)
      (translate (-0.2 2.1 0.4)
        (scale (0.1 0.1 0.1)
          (sphere1))))

    ;; Right eye (sphere)
    (rgb (1 1 1)
      (translate (0.2 2.1 0.4)
        (scale (0.1 0.1 0.1)
          (sphere1))))

    ;; Nose (pyramid)
    (rgb (0 0 0)
      (translate (0 1.9 0.5)
        (rotate 1.57 0 0)
          (scale (0.1 0.1 0.1)
            (pyramid))))

    ;; Tail (cylinder)
    (rgb (0.3 0.3 0.3)
      (translate (0 -2.0 0)
        (rotate 0 0 1.57)
          (scale (0.1 1.5 0.1)
            (cylinder)))))
)
