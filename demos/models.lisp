(load "ai-models.lisp")
(defvar square-model '(polygon :3d-triangle-strip (0 0 0 1 0 0 0 1 0 1 1 0)))
(defvar square
	 '(scale (bind args)
		(square-model)))

(defvar text
  '(offset (0 0.0 0)
	 (scale (0.01 0.01 0.1)
	  (text-base (bind (car args)))
			  )))



(define cube-model '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 1 0
                                                 1 1 0
                                                 0 1 1
                                                 1 1 1
                                                 0 0 1
                                                 1 0 1
                                                 0 0 0
                                                 1 0 0
                                                 ;; side 1
                                                 1 0 0
                                                 1 0 1
                                                 1 1 0
                                                 1 1 1
                                                 ;; step to side 2
                                                 0 1 1
                                                 ;; side 2
                                                 0 1 1
                                                 0 0 1
                                                 0 1 0
                                                 0 0 0
                                                 
                                                 )))

(define cube-2
    '(translate (-0.5 -0.5 -0.5)
               (ref cube-model)))
(define upcube
    '(translate (-0.5 0 -0.5)
               (ref cube-model)))
(define downcube
    '(translate (-0.5 -1 -0.5)
               (ref cube-model)))

(define cube1 cube-model)

(defvar pyramid
  '(polygon :3d-triangle-strip (-0.5 0 0.5   0 1 0   0.5 0 0.5   0 1 0   0.5 0 -0.5   0 1 0   -0.5 0 -0.5   0 1 0   -0.5 0 0.5)
))

(define tile-model '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 0 1
                                                 1 0 1)))
(define right-tile
  '(translate (0.0 -0.0 -0.5)
    (ref tile-model)))

(define z-tile
  '(translate (-0.5 -0.0 -0.0)
    (ref tile-model)))


(define tile-model-2
  '(translate (-0.5 0.0 -0.5)
    (ref tile-model)))


(define square-model2 '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 1 0
                                                    1 1 0)))

(defvar circle-model
  '(polygon :3d-triangle-strip (bind (gen-circle (car args)))))


(define z-square
  '(translate (-0.5 -0.5 0.0)
    (ref square-model2)))

(defun points-to-strip (points)
  (let ((strip nil))
    (loop (cddr points)
       (let ((ax (pop points))
             (ay (pop points))
             (bx (car points))
             (by (cadr points)))
         (let ((dx (- bx ax))
               (dy (- by ay)))
           (let ((len (v2-len dx dy)))
             (set! dx (/ dx len))
             (set! dy (/ dy len)))
           (let ((ty (* dx 0.25))
                 (tx (* (- 0.0 dy) 0.25)))
             (let ((nx1 (+ ax tx))
                   (ny1 (+ ay ty))
                   (nx2 (- ax tx))
                   (ny2 (- ay ty)))
               (set! strip (list* ny2 nx2 ny1 nx1 strip)))))))
    (println strip)
    (reverse! strip)))

(println (list-to-array '(1 2 3)))
(defun extrude-2d-path (strip thickness)
  (let ((out nil)
        (len (/ (length strip) 2))
        (strip2 (list-to-array strip))
        (z (- 0.0 thickness)))
    (do-times len
      (lambda (x)
        (let ((i2 (* x 2))
              (x (vector-ref strip2 i2))
              (y (vector-ref strip2 (+ i2 1))))
          (set! out (list* z y x 0.0 y x out)))))
    (reverse! out)
    ))


(defun generate-sphere-single-draw (radius steps)
  (let ((vertex-list (list))
         (step-phi (/ pi steps))
         (step-theta (/ (* 2 pi) steps)))
    ;; Generate vertices for triangle strip
    (dotimes! i steps ;; vertical steps
      (dotimes! j (+ steps 1) ;; horizontal steps
        (let ((phi1 (* i step-phi))
              (phi2 (* (+ i 1) step-phi))
              (theta (* j step-theta)))
			 (let(
               (x1 (* radius (sin phi1) (cos theta)))
               (y1 (* radius (cos phi1)))
               (z1 (* radius (sin phi1) (sin theta)))
               (x2 (* radius (sin phi2) (cos theta)))
               (y2 (* radius (cos phi2)))
               (z2 (* radius (sin phi2) (sin theta))))
				(let ((seg (list x1 y1 z1 x2 y2 z2)))
				  (println seg)
				  
				  (set! vertex-list (cons z2 (cons y2 (cons x2 (cons z1 (cons y1 (cons x1 vertex-list)))))))
				  ;(println '>> vertex-list)

				  )))
		  )
		(when (< i (1- steps))
        (let ((phi2 (* (+ i 2) step-phi))
              (theta 0))
			 (let (
               (x2 (* radius (sin phi2) (cos theta)))
               (y2 (* radius (cos phi2)))
               (z2 (* radius (sin phi2) (sin theta))))
				(set! vertex-list (cons z2 (cons y2 (cons x2 (cons z2 (cons y2 (cons x2 vertex-list)))))))
		  )
		)))

	 (reverse! vertex-list)))


(defvar sphere1
  '(sdf2 (:size 1 :resolution 0.25)
	 (rgb (1 1 1)
	  (sphere 0.9))))

(defvar upsphere
  '(plane
	 (height-sdf (4 4) (8 8)
	  (rgb (1 1 1);(offset (0 -1 0)
				  (sphere 0.9)))));)
	  
(defvar upsphere '(offset (0 1 0)
						 (sphere1)))

(defvar sphere2 (list 'polygon :3d-triangle-strip (generate-sphere-single-draw 1.0 16)))

(println sphere2)
