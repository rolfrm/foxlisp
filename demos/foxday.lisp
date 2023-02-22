(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(load "swank.lisp")
(print "FOX FACTORY")

(when nil
  (let ((swnk (swank-server-new 8810)))
    (println swnk)
    (loop t
          (swank-server-update swnk)
          (thread:sleep 0.1))))

(define win (foxgl:create-window (integer 800) (integer 800)))
(define swnk (swank-server-new 8810))
(foxgl:set-title win "Demo 1")
(foxgl:make-current win)
(foxgl:load-font "./DejaVuSans.ttf" (integer 22))

(define square-model '(polygon :2d-triangle-strip (0 0 1 0 0 1 1 1)))

(define start-time (foxgl:timestamp))
(defun update ()
  
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
  
  (foxgl:clear)
  (let ((dt (math:pow (sin (* 0.0000001 (- (foxgl:timestamp) start-time))) 10.0)))
    
    (foxgl:render-model
     '(scale (4.0 4.0 1.0)
       (translate (-0.225 -0.225 0.0)
        (rgb (0.2 0.2 0.2)
         (for i (0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
          (for j (0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
           (scale (0.05 0.05)
            (rgb ((bind (+ 0.5 (* 0.5 (sin (+ (* 3.14 dt) i j)))))
                  (bind (+ 0.5 (* 0.5 (sin (+ (* 3.14 dt 1.0) i j 1.5)))))
                   1.0 1.0)
             
             (translate ((bind i) (bind j) 0)
              (scale ((bind (+ 0.5 (* dt 0.95)))
                      (bind (+ 0.5 (* dt 0.95))) 1.0)
               (rotate (0 0 (bind (* pi (+ dt (* dt i)  (* dt j)))))
                (translate (-0.5 -0.5 0.5)
                 (ref square-model)))
               
               ))))))))
       
       )))
  
  
  (foxgl:swap win)
  (foxgl:poll-events)
  (lisp:collect-garbage)
  )



(println "starting!");
(foxgl:make-current win)
(loop t
      (with-exception-handler
          (progn
            (update))
        (lambda (x) ()))
      (swank-server-update swnk)
      )

