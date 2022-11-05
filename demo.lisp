;;to profile: ./run foxday2.lisp | sort | uniq -c |sort -n

(define window-title "funky")
(load "models.lisp")

(defvar +dt+  5)
(defvar +dt2+ 0.12)
(defvar zoom 0.08)
(defvar time2 0.0)
(defvar real-time 0.0)
(defvar guy-move 2.0)
(defvar move-x 0.0)
(defvar move-y 0.0)
(defvar turn 0.0)
(defvar turned 0.0)
;(defvar sdf1 (test:poly))
(define ball-position (list 0 0 0))

(defvar game-update
  (let ((mx nil) (my nil))
    (lambda (events)
      (println ball-position)
      (incf time2 (+ 0.0005 (* (+ 1.0 (sin time2)) 0.0025)))
      (incf real-time 0.016)
      (incf turned (* 0.16 turn 0.1))
      (set! guy-move (* 1.0 real-time))
      (set-car! ball-position (+ move-x (car ball-position)))
      (set-car! (cddr ball-position) (+ (- 0 move-y) (car (cddr ball-position))))
      (update-ball-position)
      (push-event (list 'frame time2))
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
                (when (eq (car x) 'key-down)
                  (when (eq (caddr x) foxgl:key-w)
                    (incf move-y 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-s)
                    (decf move-y 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-a)
                    (decf move-x 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (incf move-x 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-left)
                    (incf turn -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-right)
                    (decf turn -1.0)
                    )
                  )
                (when (eq (car x) 'key-up)
                  (when (eq (caddr x) foxgl:key-w)
                    (incf move-y -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-s)
                    (decf move-y -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-a)
                    (decf move-x -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (incf move-x -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-left)
                    (decf turn -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-right)
                    (incf turn -1.0)
                    )
                  )
      ))))

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
    '(bake ;arm
      (scale (1.2 1.2 1.2)
       (scale (0.6 0.3 0.3)
        (translate (0.0 -0.0 -0.5)
         (ref cube-model)))
       (translate (0.6 0.0 -0.1)
        (rotate (0 0 -0.1)
         (scale (0.4 0.2 0.2)
          (ref cube-model)))
        (translate (0.35 -0.05 0)
         (rotate (0.0 0 -0.2)
          (scale 0.2
           (ref cube-model)))
         )
        ))))
(define leg-tilt 0.25)
(define guy-leg
    '(leg
      (translate (0 -1 0)
       (rgb (0.4 0.4 0.6)
        (scale (0.2 1.0 0.2)
         (ref cube-model)))
        (translate (0 -0.22 0)
         (rotate ((bind (* 0.5 leg-tilt)) 0 0)
          (scale (0.3 0.2 0.5)
           (rgb (0.7 0.3 0.3)
            (ref cube-model)))))
       )))
(defun blink(x)
  ;; blink every 5 sec
  (set! x (* 20.0 (mod x 5.0)))
  (if (< x 95.0)
      1.0
      (/ (abs (- x 97.5)) 2.5)))


(define guy
    '(body
      (blend ;shadow
       (rgb (0 0 0 0.15)
        (translate (-0.5 0.6 -0.5)
         (ref tile-model))))
      (translate (0 2.5 0)
      (rgb (0.3 0.3 0.3) 
       (translate (-0.5 0.0 -0.1)
        (scale (1.0 1.0 0.5)
>         (rgb (0.6 0.4 0.4)
        (ref cube-model)))) ;upper body
       (rgb (0.4 0.4 0.6)
       
       (translate (-0.5 -0.6 -0.1)
        (scale (1.0 0.5 0.5)
         (ref cube-model)))) ;lower body
       )
      (translate (0 0 0.3)
       (rotate (0 0 (bind (* 0.1 (sin (* 1.0 guy-move)))))
        (rgb (0.7 0.6 0.5)
         (translate (-0.375 1.1 -0.5)
          
          (scale (0.75 0.75 0.75)
           (ref cube-model)))) ;head
        (rgb (1.0 1.0 1.0)
         ;right eye
         (translate (0.25 1.3 0.20)
          (scale (0.1 (bind (* (blink real-time) 0.2)) 0.1)
           (ref cube-model)))
         ;; left eye
         (translate (-0.15 1.3 0.20)
          (scale (0.1 (bind (* (blink real-time) 0.2)) 0.1)
           (ref cube-model)))
         (bake
         (translate (0.15 1.55 0.20)
          (rgb (0 0 0)
           (scale (0.2 0.1 0.1)
            (ref cube-model))))

         ;; left eyebrow
         (translate (-0.25 1.55 0.20)
          (rgb (0 0 0)
           (scale (0.2 0.15 0.1)
            (ref cube-model))))
         ;; hair
         (translate (-0.45 1.8 -0.6)
          (rgb (0 0 0)
           (scale (0.9 0.2 0.9)
            (ref cube-model))))
         
         ))))

       (rgb (0.7 0.6 0.5)
        (translate (0.5 0.8 0.2)
         (rotate ((bind (* -10.0 guy-move)) 0.0 -1.5)
          (ref guy-arm)))
        (translate (-0.5 0.8 0.2)
         (scale (-1.0 1.0 1.0)
          (rotate ((bind (* 10.0 guy-move)) 0 -1.0)
           (ref guy-arm)))))
       
       (let ((leg-tilt (+ (* -0.5 (sin (* 10.0 guy-move))) 0.2)))
         (translate (0.3 -0.6 0)
                    (rotate ((bind leg-tilt) 0.0 0.0)
                            (ref guy-leg))))
       (let ((leg-tilt (+ (* 0.5 (sin (* 10.0 guy-move))) 0.2)))
       
         (translate (-0.3 -0.6 0)
                    (scale (-1.0 1.0 1.0)
                           (rotate ((bind leg-tilt) 0.0 0.0)
                                   (ref guy-leg)))))
       
       )))

(define fox-color '(0.9 0.25 0.25 1))

(define fox-leg '
    (bake
     (scale (0.1 0.5 0.1)
            (ref cube-2)
            )
     (translate (0 -0.35 0)
                (rgb (0 0 0)
                     (scale (0.1 0.2 0.1)
                            (ref cube-2))
                     )
                )
     ))

(define fox-tail
    '(bake ;tail
      (rgb (bind fox-color)
    (rotate (0.7 0.7 0)
     (scale (0.2 0.2 0.2)
      (ref cube-2)))
    (translate (0.0 0.2 -0.2)
     (rotate (0.7 0.7 0)
      (scale (0.3 0.3 0.3)
       (ref cube-2))))
    (translate (0.0 0.2 -0.5)
     (rgb (1.0 0.6 0.6)
      (rotate (0.7 0.7 0)
       (scale (0.25 0.25 0.25)
        (ref cube-2)))))
    )))

(define fox-guy

    '
    (translate (0 0 -0.25)
               (blend ;; shadow
                (translate (-0.3 -0.8 -0.7)
                          (scale (0.5 1.0 1.2) 
                                 (rgb (0.0 0.0 0.0 0.15)
                    (ref tile-model)))))
    (rgb (bind fox-color)
         (bake
          (scale (0.5 0.5 1.0)
                 (ref cube-2))
          (translate (0.0 -0.1 0.05)
                     (scale (0.25 0.4 1.0)
                            (rgb (1.0 1.0 1.0)
                                 (ref cube-2)))))

      ;; head
      (bake ;(bind fox-color)
      (translate (0 0.3 0.5)
       (scale (0.5 0.5 0.5)
        (rotate (0.7 0.7 0)
         (ref cube-2)
         (rgb (0 0 0)
          (rgb (1 1 1)
          (translate (0.35 0.35 0.35)
           (scale 0.33
            (ref cube-2))))
          ;;eyes
          (rotate (0.0 -0.85 0.0)
          (translate (-0.3 0.5 0.0)
           (rotate (0 -1.7 0.0)
            (scale (0.33 0.1 0.1)
             (ref cube-2))))
          
          (translate (0.3 0.5 0.0)
           (rotate (0 1.7 0.0)
            (scale (0.33 0.1 0.1)
             (ref cube-2))))
           ;; ears
           (rgb (bind fox-color)
            (translate (0.45 0.3 -0.45)
             (rotate (0.4 1 0.0)
              (scale 0.4
               (ref cube-2)))))
           (rgb (bind fox-color)
            (translate (-0.45 0.3 -0.45)
             (rotate (0.4 1 0.0)
              (scale 0.4
               (ref cube-2)))))
          ))
                ))))
         (let ((leg-angle (sin (* 10.0 fox-move))))
         (translate (0.2 -0.3 0.3)
                    (rotate ((bind leg-angle) 0 0)
                            (ref fox-leg)))
         (translate (-0.2 -0.3 0.3)
                    (rotate ((bind (- 0 leg-angle)) 0 0)
                            (ref fox-leg)))
           (translate (0.2 -0.3 -0.4)
                      (rotate ((bind (- 0 leg-angle)) 0 0)
                    
                              (ref fox-leg)))
           (translate (-0.2 -0.3 -0.4)
                      (rotate ((bind leg-angle) 0 0)
                              (ref fox-leg)))
           
           (translate (-0.0 0.3 -0.5)
                      (rotate (0 0 (bind leg-angle))
                    (ref fox-tail)))
  
  ))))

(define chair
    '(chr
      (blend ;; shadow
       (translate (-0.0 -0.5 -0.0)
        (scale (1 1.0 1) 
         (rgb (0.0 0.0 0.0 0.1)
          (ref tile-model)))))
      (bake
       (rgb (0.6 0.35 0.25)
        (translate (0 0 0)
         (scale (0.1 1.0 0.1)
          (ref cube-2)))
        (translate (1 0 0)
         (scale (0.1 1.0 0.1)
          (ref cube-2)))
        (translate (1 0.5 1)
         (scale (0.1 2.0 0.1)
          (ref cube-2)))
        (translate (0 0.5 1)
         (scale (0.1 2.0 0.1)
          (ref cube-2)))
        
        (translate (0.5 0.5 0.5)
         (scale (1.1 0.1 1.1)
          (ref cube-2)))

        (rgb (0.2 0.3 0.2)
         (translate (0.5 0.55 0.5)
          (scale (0.9 0.1 0.9)
           (ref cube-2))))
        
        (for i (0 -2 -4 -6)
         (translate (0.5 1.5 1)
          (scale (1.1 0.1 0.1)
           (translate (0 (bind i 0))
            (ref cube-2)))
          ))))))


(defun gen-chair-items (n)
  (let ((l nil))
    (dotimes! i n
       (push! l (* 2 pi (/ (rational i) (rational n)))))
    l))
      
(defvar chair-items (gen-chair-items 7))
(defvar world-model-sdf '(translate (0 0 100)
       (soft 0.0
        (rgb (0.3 0.8 0.3)
         (subtract 0.0
          (rgb (0.3 0.8 0.3)
          
         
         (translate (0 -1000  0)
          (subtract 0.0
           (rgb (0.3 0.8 0.3)
            (sphere 1000.0))
           
           ;(translate (50.0 1000.0 -50.0)
           ; (sphere 30.0))
           
           
           ))
         
         ;(translate (50 -48 -50)
         ; (aabb 40.0 40.0 40.0))
         (sphere-bounds
          (rgb (0.25 0.75 0.25)
               (translate (0 -99 -100)
                (sphere 100.0))
               (translate (50 -99 -200)
                (sphere 100.0))
               (translate (-50 -99 -190)
                (sphere 110.0))))

          (sphere-bounds
         (rgb (3.14 0.4 0.4)
               
          (translate (0 -40 -250)
           (rotate (0 0.5 1)
            (aabb 40 40 40)))
         (translate (65 -55 -250)
          (rotate (0 0.5 1)
           (aabb 40 40 40)))))
         
          )
          
          
          (translate (58.0 0.0 -100.0)
           (rotate (1.4 0.0 0.0)
            (translate (0 -500 0)
             (aabb 5.0 1000.0 5.0)))
           )
          )

            ;)
         
         
        
        (sphere-bounds
         (rgb (0.4 0.4 0.4)
          (translate (0 0 -50)
           (aabb 10 10 10))
          (translate (6 15 -53)
           (aabb 10 12 8))
          (translate (-10 15 -53)
           (aabb 5 5 5))
         
          ))
         
         ))))
(defvar world-model-sdf2
  '(rgb (1 0 0)
    (rotate (0.9 0.9 0.9)
     (aabb 10.0 10.0 10.0))
       ))
(defvar world-model
  '(sdf :size 25  :resolution 2 :clip (bind ball-position)
    (bind world-model-sdf)
    ))
(defvar sphere-model
  '(sdf :size 5 :resolution 0.2
    (rgb (1 1 1)
     (sphere 1.0))))
(defvar test-sdf (foxgl:build-sdf world-model-sdf (lisp:get-current-scope!!)))
(defun update-ball-position()
  ;;(println ball-position)
  (let ((d (sdf:dist test-sdf ball-position)))
    
    (let ((y2 (- (cadr ball-position) (min 1 d))))
      (set-car! (cdr ball-position) y2))) 
  
  )
(set! ball-position (list 0 0 0))

(define model
    '(view :perspective (1.0 (bind foxgl:aspect-ratio) 0.01 1000.0)
      (depth
       (translate (0 0  (bind (+ -20 zoom)))
        
        (rotate (0.4 (bind turned) 0)
         (translate ((bind (- 0 (car ball-position))) (bind (- 0 (cadr ball-position)))  (bind (- 0 (caddr ball-position))))
          (bind world-model)

          (translate (65 -55 -150)
           (rotate (0 0.5 1)
            (scale (40 40 40)
             (scale 2
              (blend
               (rgb (1 1 1 0.5)
                ;(ref cube-2)
                )))

             )))

          
          (sea;blend
           (rgb (0.4 0.4 0.9 1)
            (translate (50 -8 50)
             (scale (60 1 60)
              (bind upcube)))))
         (rgb (1 1 1)
          (translate (bind ball-position)
           (translate (0 1 0)
            (translate (0 -2 0)
             (blend
              (rgb (0 0 0 0.5)
               (ref sphere-model)

               )))
            (ref sphere-model)
            )))
         (translate (0 5 0)
           (translate (0 0 -100)
          (rotate (0 0 0)
           )))

        (translate (0 0 -50)
         (rgb (1.0 1.0 0.5)
          (rotate (0 0.5 0)
           (ref cube-model))
          ))
         (bake
         (translate (0 50 -300)
         (rgb (1.0 1.0 1.0)
          (rotate (0 0.0 0)
           (scale 50
           (ref cube-model)))
          ))
         
         (translate (40 40 -300)
         (rgb (1.0 1.0 1.0)
          (rotate (0 0.0 0)
           (scale 25
           (ref cube-model)))
          ))

         (translate (100 53 -380)
          (rgb (1.0 1.0 1.0)
          (rotate (0 0.0 0)
           (scale 14
           (ref cube-model)))
          ))
          )
         
        (translate (0 0.0 -500.0)
         (rgb (0.9 0.9 1.0)
          (rotate (0.0 0.0 0.0)
           (scale (2000.0 2000.0 1.0)
            (translate (-0.5 -0.5)
             (ref square-model)))))
         )


          
          
        

         ))))))
