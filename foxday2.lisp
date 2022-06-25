(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(load "tree-model.lisp")
(lisp:collect-garbage)

(defun update ()
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
  (foxgl:clear)
    
  (let ((first (lisp:count-allocated)))
    (foxgl:render-model model)
    (when nil
  
      (print "allocated: ")
      (print first)
      (print " ")
      (println (lisp:count-allocated))
      ))
  (foxgl:swap win)
  (foxgl:poll-events)
  (lisp:collect-garbage)
  )


(define ld50:initialized nil)
(define win nil)
(define swnk nil)
(defun ld50:initialize()
  (set! win (foxgl:create-window (integer 800) (integer 800)))
  (foxgl:make-current win)
  (foxgl:load-font "DejaVuSans.ttf" (integer 22))
  (foxgl:set-title win "Tree")
  )

(load "swank.lisp")
  
(unless lisp:*web-environment*
  (set! swnk (swank-server-new 8810))

  (ld50:initialize)
  (foxgl:make-current win)
  
  (loop t
       (with-exception-handler
           (progn
             (update)
             (when swnk
               (swank-server-update swnk))
        
             )
         (lambda (x) ()))
                                        ;(swank-server-update swnk)
       ))

(defun lisp:*web-update* ()
  (unless ld50:initialized
    (set! ld50:initialized t)
    (ld50:initialize))
  
  (foxgl:make-current win)
  (with-exception-handler
      (update)
    (lambda (x)
      (println x)
      (thread:sleep 0.5)
      ))
  
  )
