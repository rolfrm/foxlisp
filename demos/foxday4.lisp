
(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(define window-title "funky")
(load "models.lisp")
(load "models-2.lisp")

(defun interpolate-angle(now target turn-speed)
  (let ((now (mod now 360.0))
		  (target (mod target 360.0)))
	 (let ((diff (- target now)))
		(when (> (abs diff) 180)
		  (set! target (+ target (if (> target now) -360.0 360.0)))

		  (set! diff (- target now))
		  )
		(mod (+ now (* (sign diff) (min turn-speed (abs diff)))) 360))))

(defvar wizard-offset (list 0.0 0.0 0.0))
(defvar wizard-walk 0.0)
(defvar wizard-angle 0.0)
(defvar wizard-speed 0.02)
(defvar map-offset (list 0.0 0.0 0.0))
(defvar tile-offset (list 0 0 0))
(defvar level '((bake2 :key (bind (car args))

					  (rgb (0.5 0.3 0)
						(rotate (0 (bind (* 90.0 (math:random 0 4))) 0)
															  
					  (offset (0 -1 0)
						(scale (1 0.9 1)
								 (upcube)))
					  (offset (0 -0.1 0)
						(rgb (0.3 0.8 0.2)
							  (scale (1 0.1 1)
										(upcube))))
						(scale 0.025
						 (cherry-tree 6 10))
					  (offset (0.321 0 0.432)
						(scale 0.025
								 (cherry-tree 6 15)))
					  (for i (range 0 50)
						(offset ((bind (math:random -0.5 0.5))
									0
									(bind (math:random -0.5 0.5))
									)
								  (scale (0.01 0.05 0.01)
											(upcube))))
					  )))))
(defvar player-wizard
  '(scale 0.05
	 (offset (0 0 0)
	  (vars ((run-cycle (* 2 wizard-walk)))
		(wizard-model))
	  (offset (-0.6 2 0)
		(scale 0.4
				 (cat-model))
		) 
	  )))

(define model '((perspective (1.0 1.0 0.1 1000.0)
	  (depth t
		(rgb
		 (1 0 0)
		 (offset (0 0 -4)
					(rotate
					 (30 45 0)
					 ;(offset (bind (println wizard-offset '<<wizard))
					 (rotate (0 (bind wizard-angle) 0)
										  (player-wizard));))
					 

					 (for i (range -6 6)
							(for j (range -6 6)
								  (offset ((bind (+ (car map-offset) i)) 0 (bind (+ (caddr map-offset) j)))
		  									 (level (bind (mod (+ (- i (car tile-offset) ) (* (- j (caddr tile-offset)) 5)) 13)))))))
  	))))))

(defvar dec-to-rad (* pi (/ 2.0 360.0))) 


(defun game-update(events)
  (let ((v-x 0.0)
		  (v-y 0.0)
		  (v-z 0.0)
		  (v-angle 0.0))
	 (when (key-down? foxgl:key-w)
		(set! v-angle 180)
		(set! v-z 0.1))
	 (when (key-down? foxgl:key-a)
		(set! v-angle 90)
		(set! v-x 0.1))
	 (when (key-down? foxgl:key-d)
		(set! v-angle -90)
		(set! v-x -0.1))
	 (when (key-down? foxgl:key-s)
		(set! v-angle 0)
		(set! v-z -0.1))
	 ;(println v-x v-z)
	 (let ((absv (math:sqrt (+ (* v-z v-z) (* v-x v-x)))))
		(when (> absv 0)
		  (let ((target-angle (/ (math:atan (- v-x) v-z) dec-to-rad)))
		  (set! wizard-walk (+ wizard-walk 0.2))
		  (set! wizard-angle (interpolate-angle wizard-angle target-angle 10))
		  (set! v-x (* wizard-speed (/ v-x absv)))
		  (set! v-z (* wizard-speed (/ v-z absv)))
		  )
		  (set! wizard-offset (list (+ (car wizard-offset) v-x) (cadr wizard-offset)
											 (+ (caddr wizard-offset) v-z)))
		  (set! map-offset (map wizard-offset (lambda (x) (math:mod x 1.0)) ))
		  (set! tile-offset (map wizard-offset (lambda (x) (integer (math:floor x))) ))
		  (println map-offset tile-offset)

		  )
		
	 ))

  )
(define should-exit nil)
(println 'load-demo)
;(load "demo3.lisp")
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
  
(defun update ()
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
(defun key-down?(key)
  (when win
	 (foxgl:key-down? win key)))
(define swnk nil)
(defun ld50:initialize()
  (set! win (foxgl:create-window (integer 800) (integer 800)))
  (foxgl:make-current win)
  (foxgl:load-font "DejaVuSans.ttf" (integer 22))
  (foxgl:set-title win window-title)
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



