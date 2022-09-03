;;to profile: ./run foxday2.lisp | sort | uniq -c |sort -n

(define window-title "funky")
(load "models.lisp")


(defvar +dt+ 3.12)
(defvar +dt2+ 0.12)
(defvar zoom 0.08)
(defvar time2 0.0)
(defvar real-time 0.0)
(defvar game-update
  (let ((mx nil) (my nil))
    (lambda (events)
      (incf time2 (+ 0.0005 (* (+ 1.0 (sin time2)) 0.0025)))
      (incf real-time 0.001)
      (push-event (list 'frame time2))
      (for-each x events
                (unless (eq (car x) 'frame)
                  (println x))
                (when (eq (car x) 'mouse-leave)
                  (set! mx nil)
                  (set! my nil))
                (when (eq (car x) 'mouse-scroll)
                  (let* ((amount (caddr x))
                         (amount2 (math:pow 1.2 amount)))
                    (set! zoom (* zoom amount2))
                    (println zoom)))
                (when (eq (car x) 'mouse-move-delta)
                  (incf +dt2+ (* -0.1 (cadr x)))
                  (incf +dt+ (* 0.1 (caddr x)))
                  )
                (when (eq (car x) 'mouse-move)
                  (let ((mx2 (rational (cadr x)))
                        (my2 (rational (cddr x))))
                    (when (and mx my)
                      (push-event (list 'mouse-move-delta (- mx2 mx) (- my2 my))))
                   
                    (set! mx mx2)
                    (set! my my2)
                 
                    ))
      ))))

(defvar dots nil)
(dotimes! i 1000
          (push! dots (* (- i 500) 0.03)))

(define start-time (foxgl:timestamp))

(defun yfunc(x2)
  (let ((x (* 0.1 (+ (* time2 40.0) x2))))
    (* 10.0 (cos (* x 3.3)) (sin x))))

(defvar square-loop
  '(line
           (point -10.0 -9.0)

           (point -10.0 9.0)
           (point -9.0 10.0)

           (point 9.0 10.0)
           (point 10.0 9.0)

           (point 10.0 -9.0)
           (point 9.0 -10.0)

           (point -9.0 -10.0)
           (point -10.0 -9.0)

           ))

(define guy-arm
    '(arm
      (scale (1.2 1.2 1.2)
     (scale (0.6 0.3 0.3)
      (translate (0.0 -0.0 -0.5)
       (ref cube-model)))
     (translate (0.6 0 0)
      (rotate (0 0 -0.1)
       (scale (0.4 0.2 0.2)
        (ref cube-model)))

      ))))

(define guy
    '(body
      (rgb (0.3 0.3 0.3) 
       (translate (-0.5 0.0 -0.5)
        (ref cube-model)) ;upper body
       (translate (-0.5 -0.6 -0.5)
        (scale (1.0 0.5 1.0)
         (ref cube-model))) ;lower body
       (rgb (0.2 0.2 0.2)
        (translate (-0.375 1.1 -0.5)
        (scale (0.75 0.75 0.75)
         (ref cube-model)))) ;head
       (rgb (1.0 1.0 0.0)
        
        (translate (0.25 1.3 0.20)
         (scale (0.1 0.2 0.1)
          (ref cube-model)))
        (translate (-0.25 1.3 0.20)
         (scale (0.1 0.2 0.1)
          (ref cube-model)))

        )

       (translate (0.5 1 0)
        (rotate (0.0 0.0 -1.5)
         (ref guy-arm)))
       (translate (-0.5 1 0)
        (scale (-1.0 1.0 1.0)
         (ref guy-arm)))
       
       )))

(define model
    '(view :perspective (1.0 1.0 0.01 1000.0)
      (depth
       (translate (-0.5 -0.5 -5.0)
        (rotate (0 (bind real-time) 0)
         (ref guy)))
       
       (translate (0 (bind (- -10 +dt2+ 0)))
        (translate (0 0 -50)
         (rgb (1.0 1.0 0.5)
          (rotate (0 0.5 0)
           (ref cube-model))
          ))
        
        (rgb (0.5 0.9 0.5)
         (translate (0 0.0 -100.0)
          (scale (10 10 10)
          (rotate (0.5 0.5 0.5)
           (ref cube-model)))
         )
         )
        (rgb (0.3 0.7 0.3)
         (translate (-0 -70.0 -400.0) 
          (scale (100 100 100)
          (rotate (1.0 1.0 1.2)
           (ref cube-model)))
         )
        )
        (translate (0 0.0 -100.0)
         (rgb (0.2 0.7 0.2)
          (rotate ((bind (* 1.0 pi_2)) 0.0 0.0)
           (scale (2000.0 2000.0 1.0)
            (translate (-0.5 -0.5)
             (ref square-model)))))
         
         )))))
