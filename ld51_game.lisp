;;to profile: ./run foxday2.lisp | sort | uniq -c |sort -n

(define window-title "Ninja Thief")
(load "models.lisp")

(defvar +dt+  5)
(defvar +dt2+ 0.12)
(defvar zoom 0.08)
(defvar time2 0.0)
(defvar real-time 0.0)
(defvar move-forward 0.0)
(defvar turn 0.0)
(defvar ang 0.0)
(defvar player-x 0.0)
(defvar player-y 0.0)
(defvar player-move 0.0)

(defvar game-update
  (let ((mx nil) (my nil))
    
    (lambda (events)
      (println (cons player-x player-y))
      (incf ang (* 0.1 turn))
      (incf player-move (* 0.1 move-forward))
      (let ((angle ang))
        (incf player-x (* -0.1 move-forward (sin angle)))
        (incf player-y (* 0.1 move-forward (cos angle)))
        )
        
        
      (incf real-time 0.016)
      (when (> +dt+ 0.01)
        (set! +dt+ 0.01))
      (for-each x events
                (unless (eq (car x) 'frame)
                  (println x))
                (when (eq (car x) 'mouse-leave)
                  (set! mx nil)
                  (set! my nil))
                (when (eq (car x) 'mouse-scroll)
                  (let* ((amount (caddr x))
                         (amount2 amount))
                    (set! zoom (+ zoom amount2))
                    (println zoom)))
                (when (eq (car x) 'key-down)
                  (when (eq (caddr x) foxgl:key-w)
                    (incf move-forward 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-s)
                    (decf move-forward 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-a)
                    (incf turn 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (decf turn 1.0)
                    )
                  )
                (when (eq (car x) 'key-up)
                  (when (eq (caddr x) foxgl:key-w)
                    (incf move-forward -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-s)
                    (decf move-forward -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-a)
                    (incf turn -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (decf turn -1.0)
                    )
                  )
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

(define start-time (foxgl:timestamp))

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

(defun blink(x)
  ;; blink every 5 sec
  (set! x (* 20.0 (mod x 5.0)))
  (if (< x 95.0)
      1.0
      (/ (abs (- x 97.5)) 2.5)))

(defvar ninja-leg
  '(leg
    (rotate (-0.0 0 0)
     (translate (0 0 0)
      
      (translate (0 -0.4 0)
       (scale (0.1 0.5 0.1)
        (ref cube1)); thigh
     
       (translate (0 -0.125 0)
        (translate (-0.025 0 -0.025)
        (scale (0.15 0.25 0.15)
         (rgb (0.9 0.9 0.9)
          (ref cube1)); knee
         ))
        (rotate (0.0 0 0)
         (translate (0.0 -0.5 0)
          (scale (0.1 0.5 0.1)
           (ref cube1))
          ; foot
           (translate (0 0.05 0.05)
            (scale (0.1 0.1 0.2)
             (ref cube1))))
      
          )
         ))
       ))))

(defvar ninja-arm
  '(arm
    (rotate (-0.0 0 0)
     (translate (0 0 0)
      
      (translate (0 -0.3 -0.1)
       (scale (0.05 0.4 0.1)
        (ref cube1)); thigh
     
       (translate (0 -0.125 0)
        (translate (-0.025 0 -0.025)
        (scale (0.1 0.15 0.1)
         (rgb (0.9 0.9 0.9)
          (ref cube1)); elbow
         ))
        (rotate (0.0 0 0)
         (translate (0.0 -0.5 0)
          (scale (0.05 0.5 0.1)
           (ref cube1))
          ; hand
          (translate (-0.05 0.0 0.0)
           (scale (0.07 0.05 0.1)
            (ref cube1))))
      
          )
         ))
       ))))

(defvar ninja-color '(0.2 0.2 0.2))
(defvar model-player
  '(ninja
    (blend
    (rgb (0 0 0 0.2)
     (scale (0.45 1 0.45)
      (translate (-0.5 0.01 -0.25)
       (ref tile-model)))))
    (rgb (bind ninja-color)
     (translate (0.25 1 0)
      (scale (-1 1 1)
       (rotate ((bind (sin player-move)) 0 0)
        (ref ninja-leg))))
     
     (translate (-0.25 1 0)
      (rotate ((bind (sin (+ 3.14 player-move))) 0 0)
       
       (ref ninja-leg)))

     (translate (0 1.25 0.05)

      (scale (0.5 0.5 0.2)
       (ref cube-2)
       )
      (translate (0 0.6 0)
       (scale (0.5 0.5 0.2)
        (ref cube-2)) ;; upper body

       (translate (0.25 0.1 -0.1)
        (rotate ((bind (+ 3.14 (* 0.3 (sin (* 1 player-move))))) 0 0)
        
         (ref ninja-arm)))

       (translate (-0.25 0.1 -0.1)
        (rotate ((bind (+ 3.14 (* 0.3 (sin (+ 3.14 (* 1 player-move)))))) 0 0)
         (scale (-1 1 1)
          (ref ninja-arm))))
      
        (translate (0 0.5 0)
         (scale (0.25 0.25 0.25)
          (ref cube-2)  ;; head
          )
         (rgb (1 1 1 )
          (translate (0 0 0.13)
          (scale (0.15 0.01 0.01)
           (ref cube-2)  ;; eyes
           )))
         )

        )
      ))))

(defvar object-1
    '(rgb (1 0 0)
      (translate (0 0.5 0)
       (scale (0.5 1 0.5)
        (ref cube-2)))))

(defvar object-2
    '(rgb (0 1 0)
      (translate (0 0.5 0)
       (scale (0.5 1 0.5)
        (ref cube-2)))))

(defvar object-3
    '(rgb (0 0 1)
      (translate (0 1.0 0)
       (scale (1.3 2.5 0.1)
        (ref cube-2)))))


(define model
    '(view :perspective (1.0 (bind foxgl:aspect-ratio) 0.01 1000.0)
      (depth
       (view :orthographic (1.0 1.0 2.0) 
       (translate (0.8 -0.7 -1)
        (rotate (0 0.0 0)
        (rotate (0.5 0 0)
         (rotate (0 (bind real-time) 0)
          (scale (0.2 0.2 0.2)
           (translate (0 0 0)
            (ref object-1))
          (translate (0 1.2 0)
           (ref object-2))
           (translate (0 2.9 0)
          (ref object-3))
         ))))))
       
       (translate (0 -1  (bind (+ -6 zoom)))
        (rotate (0.9 0 0)
         (translate ((bind (* -1.0 player-x)) 0 (bind (* -1.0 player-y)))
         (rgb (1 1 1)
          (translate (-5 0 -5)
           (scale (10 0 10)
            (ref tile-model)
            )))
         
         (rgb (0 0 0)
          (translate ((bind player-x) 0 (bind player-y))
           (rotate (0 (bind ang) 0)
            (ref model-player))))
          (translate (1 0 0)
           (ref object-1))

          (translate (-2 0 3)
           (ref object-2))
          (translate (-5 0 0)
           (rotate (0 (bind (* 0.5 pi)) 0)
            (ref object-3)))

         ))))))
        

