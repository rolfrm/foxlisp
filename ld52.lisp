(println 'eval1)
(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(define window-title "funky")
(load "models.lisp")
(define model '())
(defun game-update(events)

  )
(define should-exit nil)
(println 'load-demo)
(load "ld52_model.lisp")
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
(defvar foxgl:aspect-ratio 1.0)
(defvar foxday-debug-state nil)
(defvar real-time 0.0)
(defvar vlen (* 8 128))
(define sample1 (make-vector vlen (float32 0)))
;(dotimes! i vlen
;  (vector-set! sample1  i (float32 (sin (* 2 pi 440 (/ (float32 i) 44100.0))))))

(println sample1)
(define sample1-loaded (audio:load-sample sample1))
(define sample2-loaded (audio:load-sample sample1))
(define sample3-loaded (audio:load-sample sample1))
(define sample4-loaded (audio:load-sample sample1))
(defvar source (audio:new-source))
(audio:source-queue source sample1-loaded)
(audio:source-queue source sample2-loaded)
(audio:source-queue source sample3-loaded)
(audio:source-play source)
(println source)
(defvar freq1 0.0)
(defvar phase 1.0)

(defun update-audio ()
  (audio:update)
 
  (let ((new-buffer (audio:source-update source)))
  	(while new-buffer
	  (audio:fill-buffer new-buffer sample1)
	  (audio:source-queue source new-buffer)
	  (set! new-buffer (audio:source-update source))
	  (println 'new-bufer)
	  )
   
	))

(update-audio)

;(audio:source-play source sample4-loaded)
(defun update ()
										;(update-audio)
  	
  (incf real-time 0.1)
  (lisp:collect-garbage)
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s)))
    (set! foxgl:aspect-ratio (/ (rational (car s)) (rational (cadr s))))
    )
  (foxgl:clear)
  (let ((events (poll-events2)))
    (for-each evt events
              (when (eq (car evt) 'char)
                (when (eq (cadr evt) 'd)
                  (lisp:debug (swap foxday-debug-state (not foxday-debug-state))))
                (when (eq (cadr evt) 'q)
                  (set! should-exit t))
                )
              )
    (game-update events))
  (let ((first (lisp:count-allocated)))
	(foxgl:init)
	(eval-scoped scope-3d model)
    
    )
  (foxgl:swap win)
  (foxgl:poll-events)
  )

(define ld50:initialized nil)
(define win nil)
(define swnk nil)
(defun ld50:initialize()
  (println 'load-window)
  (set! win (foxgl:create-window (integer 800) (integer 800)))
  (println 'make-current)
  (foxgl:make-current win)
  (foxgl:load-font "DejaVuSans.ttf" (integer 22))
  (foxgl:set-title win window-title)
  (println 'grap-mouse!)
  ;(foxgl:window-cursor-mode win :disabled)
  )

(defun lisp:print-stack-trace()
  (println '----stack-trace----)
  (for-each y (deref-pointer lisp:++current-error-stack++)
            (print (car y))
                      
            (let ((cd (lisp:code-location y)))
              (when cd
                (print (car cd))
                (print ":")
                (print (cadr cd))
                ))
            (println "")
            ))

(unless lisp:*web-environment*
  (load "swank.lisp"))


(unless (or lisp:*web-environment*)
  (set! swnk (swank-server-new 8810))

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

(define last-size nil)

(defun lisp:*web-update* ()
  (unless ld50:initialized
    (set! ld50:initialized t)
    (ld50:initialize))
  
  (foxgl:make-current win)
  (let ((s (foxgl:get-web-canvas-size)))
    (when (or (eq nil last-size)
              (> (+ (abs (-  (car s) (car last-size)))
                    (abs (-  (cdr s) (cdr last-size))))
                 4))
      (foxgl:window-set-size win (car s) (cdr s)))
    (set! last-size s)
    
    )
  
  (with-exception-handler
      (update)
    (lambda (x)
      (println 'exception-handler)
      (println x)
      (thread:sleep 0.5)
      ))
  
  )
