;; These models are generated using chatGPT since i was too lazy to do it myself.

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

