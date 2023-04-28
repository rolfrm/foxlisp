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

(define tile-model-2
  '(translate (-0.5 -0.5 -0.5)
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
