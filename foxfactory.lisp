(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(load "swank.lisp")
;(thread:start (lambda () (swank:start-server)))
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

(println belts)

(map! (lambda (x) (vector-set! (cdr belts) (cadr x) x)) goods)
(progn
  (map! (lambda (x)
        (let ((i (cadr x))
              (state (vector-ref (car belts) (cadr x)))
              (other-obj (vector-ref (cdr belts) (+ 1(cadr x)))))
          (when other-obj
            (let ((max-state (vector-ref (car belts) (+ (cadr x) 1))))
              (incf state (min 0.2 max-state))
              (println (cons i state))
              ))
          (unless other-obj
            (incf state 0.2)
            )
          (when (> state 0.999)
            (if other-obj
                (set! state 1.0)
                (progn
                  (set! state 0.0)
                  (vector-set! (cdr belts) (+ 1 (cadr x)) x)
                  (vector-set! (cdr belts) (cadr x) nil)
                  (set-car! (cdr x) (+ 1 (cadr x))))
                )
            )
                 
               
          (vector-set! (car belts) (cadr x) state)
          ))
        goods)
  (println (cdr belts)))
    
(defun update ()
  (foxgl:clear)
  (render-model '(color :rgb (1 1 1)
                  (transform :translate (-0.5 -0.5 0)
                   (ref square-model))))
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

