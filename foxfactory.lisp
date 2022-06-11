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
(foxgl:set-title win "Fox Driver")
(foxgl:make-current win)
(foxgl:load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))

(define square-model '(polygon :2d-triangle-strip (0 0 1 0 0 1 1 1)))
(define belts
  (cons (make-vector 10 0.0)
        (make-vector 10 nil)))

(define goods
  '((tree 0)
    (tree 1)
    (iron 2)))

(defun start-belts ()
  (set! belts
        (cons (make-vector 10 0.0)
              (make-vector 10 nil)))

  (set! goods
        (copy-list-deep '((tree 0)
                          (tree 1)
                          (iron 3))))


  (println belts)
  
  (map! (lambda (x) (vector-set! (cdr belts) (cadr x) x)) goods)
  )
(start-belts)

(defun belt-update(t)
  (map! (lambda (x)
          
          (let ((i (cadr x)))
            (when (< i (vector-length (car belts)))
              (let
                  ((state (vector-ref (car belts) (cadr x)))
                   (other-obj (vector-ref (cdr belts) (+ 1 (cadr x)))))
              (when other-obj
                (let ((max-state (if (= (+ i 1) (vector-length (car belts))) 0.0 (vector-ref (car belts) (+ (cadr x) 1)))))
                  (incf state (min t max-state))
                  ))
              (unless other-obj
                (incf state t)
                )
              (when (> state 0.999)
                (if other-obj
                    (set! state 1.0)
                    (progn
                      (set! state 0.0)
                      (vector-set! (car belts) (cadr x) state)
                      (vector-set! (cdr belts) (+ 1 (cadr x)) x)
                      (vector-set! (cdr belts) (cadr x) nil)
                      (set-car! (cdr x) (+ 1 (cadr x))))
                    )
                )
               
              (vector-set! (car belts) (cadr x) state)
              ))))
        goods))
(define start-time (foxgl:timestamp))
(defun update ()
  
  (belt-update 0.01)
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
    
  (foxgl:clear)
  
  (let ((time (* 0.000001 (- (foxgl:timestamp) start-time )))
        (offset 0.0)
        )

    (dotimes! i 10
              (foxgl:render-model
               '(rgb (0.0 0.4 0.0)
                 (scale (0.1 0.1 0.1)
                  (translate ((bind i) 0.0 0.0)
                   (transform :translate (-0.5 -0.6 0.0) :scale (0.95 1.20) 
                    (ref square-model))))))
              )
                
    (set! offset 0.0)
    (when t
    (dotimes! i 10
              (when (vector-ref (cdr belts) i)
                (foxgl:render-model
                 '(rgb (0.4 0.4 0.0)
                   (scale (0.1 0.1 0.1)
                    (translate ((bind (+ i (vector-ref (car belts) i))) 0.0 0.0)
                     (transform :translate (-0.5 -0.45 0.0) :scale (0.90 0.90 0.5)
                      (ref square-model))))))
                  ))
    )
    )
    
    
  (foxgl:swap win)
  (foxgl:poll-events)
  (lisp:collect-garbage)
  )

(defun run-update ()
  (update)
)

(defun a-test ()
  (println 'x))
(defun b-test()
  (a-test))

(println "starting!");
(foxgl:make-current win)
(loop t
      (with-exception-handler
          (progn
            (run-update))
        (lambda (x) ()))
      (swank-server-update swnk)
      )

