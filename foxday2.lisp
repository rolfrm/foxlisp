(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(load "swank.lisp")

(when nil
  (let ((swnk (swank-server-new 8810)))
    (println swnk)
    (loop t
          (swank-server-update swnk)
          (thread:sleep 0.1))))

(define win (foxgl:create-window (integer 800) (integer 800)))
(define swnk (swank-server-new 8810))
(foxgl:set-title win "Demo 2")
(foxgl:make-current win)
(foxgl:load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))

(define square-model '(scale (0.5 0.5)
                       (polygon :2d-triangle-strip (-1 -1 1 -1 -1 1 1 1))))


"(defun gen-circle(n)
  (let ((v (list )))
    (dotimes! i n
        (if (is-odd i)
"
(define start-time (foxgl:timestamp))
(defun update ()
  
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
  
  (foxgl:clear)
  (let* ((dt (* 0.000001 (- (foxgl:timestamp) start-time)))
         (sway (* 0.1 (sin dt))))
    (foxgl:render-model
     '(scale (2.0 2.0)
       (rgb (0.6 0.6 1.0)
        (bind square-model))
       (translate (0 -0.55)
        (rgb (0.6 1.0 0.6)
         (bind square-model)
         (translate (0.3)
          (rotate (0 0 0.1)
           (bind square-model)))
         ))
       )
     )

    (foxgl:render-model
     '(scale (0.25 0.25 0.25)
       (translate (-0.225 -0.225 0.0)
        (rotate (0 0 0)
        (rgb (0.7 0.4 0.4)
         (bind square-model)
         (for i (-0.5 0.5)
          (rotate (0 0 (bind (+ i sway)))
           (translate (0 0.9 0)
            (scale (0.7 1.0)
             (bind square-model))
            (for i (-0.5 0.5)
             (scale (0.8 0.8)
              (rotate (0 0 (bind (+ i sway)))
               (translate (0 0.8 0.0)
                (scale (0.7 1.0)
                 (bind square-model))
                (for i (-0.5 0.5)
                 (scale (0.8 0.8)
                  (rotate (0 0 (bind (+ i sway)))
                   (translate (0 0.8 0)
                    (scale (0.7 1.0)
                     (bind square-model))
                    (for i (-0.5 0.5)
                     (scale (0.8 0.8)
                      (rotate (0 0 (bind i))
                       (translate (0 0.8 0)
                        (scale (0.7 1.0)
                         (bind square-model))
                        (for i (-0.5 0.5)
                         (scale (0.8 0.8)
                          (rotate (0 0 (bind (+ sway i)))
                                  (translate (0 0.8)
                                             (rgb (0.33 0.66 0.33)
                                             (scale (0.7 1.0)
                                                    (bind square-model))
                                             (for i (-0.5 0.5)
                                                  (scale (0.8 0.8)
                                                         (rotate (0 0 (bind (+ sway i)))
                                                                 (translate (0 0.8)
                                                                            (scale (0.7 1.0)
                                                                                   
                                                                                   (bind square-model))
                
                                                                            )))))
                                             ))))
                        ))))
                    ))))
                ))))
            )))
         
         ))))
     ))
  
  (foxgl:swap win)
  (foxgl:poll-events)
  (lisp:collect-garbage)
  )

(foxgl:make-current win)
(loop t
      (with-exception-handler
          (progn
            (update))
        (lambda (x) ()))
      (swank-server-update swnk)
      )

