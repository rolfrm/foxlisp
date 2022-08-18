(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")

(define window-title "funky")
(define square-model '(polygon :2d-triangle-strip (-1 -1 1 -1 -1 1 1 1)))
(define model '())
(defun game-update(events)

  )
(define should-exit nil)
(load "demo.lisp")
(lisp:collect-garbage)
(defvar +custom-events+ ())
(defun push-event (event)
  (push! +custom-events+ event)
  )

(defun poll-events2 ()
  (let ((evts (foxgl:get-events)))
    (let ((evts2 (map evts (lambda (evt)
                             (let* ((tsl (cddr evt))
                                    (ts (car tsl))
                                    (tsl (cdr tsl))
                                    (e (car tsl)))
                               (cons e (cdr tsl)))))))
      (concat evts2 (swap +custom-events+))
      )))

(defvar foxday-debug-state nil)
(defun update ()
  (lisp:collect-garbage)
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
  (foxgl:clear)
  (let ((events (poll-events2)))
    (for-each evt events
              (when (eq (car evt) 'char)
                (print evt)
                (when (eq (cadr evt) 'd)
                  (lisp:debug (swap foxday-debug-state (not foxday-debug-state))))
                (when (eq (cadr evt) 'q)
                  (set! should-exit t))


                )
              ;(println evt)
              )
    (game-update events))
  (let ((first (lisp:count-allocated)))
    ;(println first)
    (foxgl:render-model model)

    )
  (foxgl:swap win)
  (foxgl:poll-events)
  )

(define ld50:initialized nil)
(define win nil)
(define swnk nil)
(defun ld50:initialize()
  (set! win (foxgl:create-window (integer 800) (integer 800)))
  (foxgl:make-current win)
  (foxgl:load-font "DejaVuSans.ttf" (integer 22))
  (foxgl:set-title win window-title)
  )
;(unless! lisp:*web-environment*
;(load "swank.lisp")

(defun lisp:print-stack-trace()
  (for-each y (deref-pointer lisp:++current-error-stack++)
            (print y)
                      
            (let ((cd (lisp:code-location y)))
              (when cd
                (print (car cd))
                (print ":")
                (print (cadr cd))
                ))
            (println "")
            ))
            
(unless lisp:*web-environment*
  ;(set! swnk (swank-server-new 8810))

  (ld50:initialize)
  (foxgl:make-current win)
  
  (loop (not should-exit)
        (with-exception-handler
            (progn
              (when swnk
                (swank-server-update swnk))
              (update)
              )
          (lambda (x)
            (lisp:print-stack-trace)
            (println x)
            (thread:sleep 0.1)
      
            ))
        )
  (println 'exiting)
  )

(defun lisp:*web-update* ()
  (unless ld50:initialized
    (set! ld50:initialized t)
    (ld50:initialize))
  
  (foxgl:make-current win)
  (with-exception-handler
      (update)
    (lambda (x)
      (println 'exception-handler)
      (thread:sleep 0.5)
      ))
  
  )
