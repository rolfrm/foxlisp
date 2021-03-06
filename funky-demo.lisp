(define window-title "funky")
(define square-model '(polygon :2d-triangle-strip (-1 -1 1 -1 -1 1 1 1)))

(defun sin2(x)
  (let ((x (sin x)))
    (* x x)))

(define start-time (foxgl:timestamp))
(define model
    '(scale (1 1 1)
      (let ((dt (* 0.000001 (- (foxgl:timestamp) start-time)))
            (dt2 (+ 0.0 (* 4.0 (sin dt))))
            (dt3 (* 0.5 (+ (sin dt) 1.0)))
            (dt4 (* 0.5 (+ (sin (* 0.5 dt)) 1.0)))
            (dt5 (* 0.5 (+ (sin (* 0.25 dt)) 1.0)))
            )
        (translate (0 0 0)
          (rgb ((bind dt5) (bind dt4) (bind dt3))
               (bind square-model)))
        (scale (0.33 0.33)
        ;(let ((dt3 (sin dt))
          (for k ((bind (* 0.5 pi_2)) (bind (* -0.5 pi_2)))
               (rotate (0 0 (bind (* k dt2)))
               (scale (0.5 0.5)
               (for i (-1 0 1)
                    (for j (-1 0 1)
                         (translate ((bind (* i dt2)) (bind (* j dt2)))
                                  (rotate (0 0 (bind (* k dt2)))
                 (scale (1 1 1.0)
                     (rgb ((bind dt3) (bind dt4) (bind dt5))
                        (bind square-model))))

        ))))))))));)

