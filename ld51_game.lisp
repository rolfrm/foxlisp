;;to profile: ./run foxday2.lisp | sort | uniq -c |sort -n

(define window-title "Ninja | Thief")
(load "models.lisp")

(defvar zoom 0.08)
(defvar time2 0.0)
(defvar real-time 0.0)
(defvar move-forward 0.0)
(defvar turn 0.0)
(defvar ang 0.0)
(defvar player-x 0.0)
(defvar player-y 0.0)
(defvar player-move 0.0)
(defvar pick-anim nil)
(defvar pick-state nil)
(defvar timer-start (foxgl:timestampf))
(defvar timer-end (+ (foxgl:timestampf) 10))

(defun delta-time ()
  (/ (- (foxgl:timestampf) timer-start) (- timer-end timer-start)))

(defvar player-body-rot 1)
(defvar drop-point-loc
  `((0 0 5) 0 drop-point))
(defvar level-objects
  `(
    ((-5.0 0 0) ,(* 0.5 pi) object-3)
    ((2.0 0 -4.5) 0 object-1)
    ((1.0 0 -4.5) 0 object-1)
    ((0.0 0 -4.5) 0 object-1)
    ((2.0 0 -2) ,pi object-1)
    ((1.0 0 -2) ,pi object-1)
    ((0.0 0 -2) ,pi object-1)
    ((-3.0 0 4.5) 0 object-2)
    ((-4.0 0 3) 0 object-2)
    ((-4.2 0 -3) 0 object-4-sofa)
    ((-2.0 0 -1) ,(* 1.5 pi) object-4-sofa)
    ((-0.5 0 -4) 0 o14-table)

    ((3.0 0 3) 0 o13-cup)
    ((0.0 1 -3) 0 o13-cup)
    ((4 0 0) 0 o15-baby)
    ((-1.8 0 1.0) ,(* 0.3 pi) o16-dog)

    
    ((4 0 -4) 0 object-5)
    ((-10 0 -9) 0 object-6)
    ((-11 0 -6) 0 object-6)
    ((-12 0 -3) 0 object-6)
    ((-10 0 -3) 0 object-7)
    ((-10 0 -6) 0 object-7)
    ((-8 0 -9) 0 object-7)
    ((-2 0 -4.9) 0 o8-picture)
    
    ((4.9 0 -0.9) ,(* 0.5 pi) o12-picture)
    ((-5 0 -5.0) 0 o9-wall)
    ((-5 0 -5.0) ,(* 0.5 pi) o10-wall)
    ((-5 0 1.0) ,(* 0.5 pi) o10-wall)
    ((-6 0 -1.0) 0 o11-wall)
    ((5 0 -5.0) ,(* 0.5 pi) o9-wall)
    )
  )
(defvar floor-tiles
  '(
    ((0 0) (10 10))
    ((-11 -2.5) (10 15))
    ((-16 -20) (10 15))
    ((-12 -12.5) (2 15))
    ((-5 -0)(4 2))
    ))

(defvar goals-list '(
                     (object-2)
                     (object-1 object-2 )
                     (object-5) (object-4-sofa)
                     (object-3)
                     (object-6 object-7)
                     (object-6 object-7)
                     (object-6 object-7)))

(defvar goal-objects (pop! goals-list));'(object-1 object-2 object-3))

(defvar picked-objects '())
(defun player-collision(px py)
  (let ((player-object `((,px 0 ,py) ,ang player)))
    (let ((col nil))
      (for-each i level-objects
                (when (foxgl:detect-collision i player-object)
                  (println (list 'collision i))
                  (set! col i)
                  ))
      col)))

(defun drop-point-collision(px py)
  (let ((player-object `((,px 0 ,py) ,ang player)))
    (let ((col nil))
      (when (foxgl:detect-collision drop-point-loc player-object)
        (println (list 'collision drop-point-loc))
        (set! col drop-point-loc)
        )
      col)))

(defun player-floor-collision (px py)
  (let ((player-object `((,px 0 ,py) ,ang player))
        (collider nil))
    (for-each f floor-tiles
       (when
          (foxgl:detect-collision-floor f player-object)
         (set! collider f)))
    
    collider
    ))
  
(defvar game-update
  (let ((mx nil) (my nil))
    
    (lambda (events)
      ;(println (- (foxgl:timestampf) timer-start))
      ;(player-collision)
                                        ;(println (cons player-x player-y))

      (incf ang (* 0.1 turn))
      (incf player-move (* 0.1 move-forward))
      (let ((angle ang))
        (let ((nx (+ player-x (* -0.1 move-forward (sin angle))))
              (ny (+ player-y (* 0.1 move-forward (cos angle))))
              (fc1 (player-floor-collision nx ny))
              (fc2 (player-floor-collision player-x player-y))
              )
          ;(println (cons (- nx player-x) (- ny player-y)))
          (when (or (not (player-collision nx ny))
                     (player-collision player-x player-y)
                     )
            (when (or fc1 (not fc2))
              (set! player-x nx)
              (set! player-y ny)))
          ))
      
      (when pick-anim
        (set! player-body-rot (- 1 (abs (- pick-anim 1.0))))
        (incf pick-anim 0.05)
        (when (and (not pick-state) (> pick-anim 1.0))
          (set! pick-state t)
          (let ((angle ang))
            (let* ((nx (+ player-x (* -0.5 (sin angle))))
                   (ny (+ player-y (* 0.5 (cos angle))))
                   (pick (player-collision nx ny))
                   (dp (drop-point-collision nx ny)))
              (when pick
                (set! level-objects (take (lambda (x) (not (eq x pick))) level-objects))
                (push! picked-objects pick)
                )
              (when dp
                (let ((thing nil))
                  (while (set! thing
                               (first (lambda (x)
                                        (first (lambda (y) (println (eq (println (caddr x)) (println y))))
                                               goal-objects))
                                      picked-objects))
                         (set! picked-objects (take (lambda (x) (not (eq x thing))) picked-objects))
                         (set! goal-objects (take (lambda (x) (not (eq x (caddr thing)))) goal-objects))
                         (when (not goal-objects)
                           (set! goal-objects (pop! goals-list))

                           )
                         (incf timer-end 10.0)
                         (set! timer-start (foxgl:timestampf))
                         (println (cons 'DROP! thing)))))
              
              (unless (or pick dp)
                (let ((set (pop! picked-objects)))
                  (when set
                    (push! level-objects set)))
                )
              
              ))
      
          )
        
        (when (> pick-anim 2.0)
          (set! pick-anim nil)
          (set! pick-state nil)

        ))
        
        
      (incf real-time 0.016)
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
                  (when (eq (caddr x) foxgl:key-space)
                    (set! pick-anim 0.0)
                    )
                  (when (eq (caddr x) foxgl:key-a)
                    (decf turn 1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (incf turn 1.0)
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
                    (decf turn -1.0)
                    )
                  (when (eq (caddr x) foxgl:key-d)
                    (incf turn -1.0)
                    )
                  )
                (when (eq (car x) 'mouse-move-delta)
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
       (rotate ((bind (sin (* 2.0 player-move))) 0 0)
        (ref ninja-leg))))
     
     (translate (-0.25 1 0)
      (rotate ((bind (sin (+ 3.14 (* 2.0 player-move)))) 0 0)
       
       (ref ninja-leg)))

     (translate (0 1.4 0.05)
      (rotate ((bind player-body-rot) 0 0)
       
      (scale (0.5 0.3 0.2)
       (ref cube-2) ;; lower-body
       )
      
      (translate (0 0.3 0)
       (rotate ((bind player-body-rot) 0 0)
        (scale (0.5 0.5 0.2)
         (translate (0 0.5 0)
          (ref cube-2))) ;; upper body

       (translate (0.25 0.1 -0.1)
        (rotate ((bind (+ 3.14 (* 0.4 (sin (* 2 player-move))))) 0 0)
        
         (ref ninja-arm)))

       (translate (-0.25 0.1 -0.1)
        (rotate ((bind (+ 3.14 (* 0.4 (sin (+ 3.14 (* 2 player-move)))))) 0 0)
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
      ))))))

(defvar object-1
  ;; chair
  '(bake (rgb (0.7 0.5 0.2)
          
     (let ((chair-leg
             '(scale (0.1 0.6 0.1)
               (ref cube-model))))
     (translate (-0.25 0 -0.25)
       (translate (0 0 0)
                  (bind chair-leg))
       (translate (0.5 0 0)
            (scale (-1 1 1)
                  (bind chair-leg)))
       (translate (0.0 0.0 0.5)
                  (scale (1 1 -1)
                   (bind chair-leg)))
       (translate (0.5 0.0 0.5)
          (scale (-1 1 -1)
                 (bind chair-leg)))
       (translate (0 0.6 0.0)
          (scale (0.5 0.1 0.5)
                 (ref cube-model))
          (translate (0.05 0.01 0.05)
             (rgb (0.3 0.3 1.0) 
                (scale (0.4 0.1 0.4)
                    (ref cube-model))))

          (translate (0 0 0)
                     (bind chair-leg))
          
          (translate (0.5 0 0)
                     (scale (-1 1 1)
                            (bind chair-leg)))
          (translate (0 0.4 0.0)
                     (scale (0.5 0.3 0.1)
                            (ref cube-model)))
          )
       
       )))))

(defvar object-2 ;; flower
    '(rgb (0.85 0.4 0.2)
      (translate (0 0.0 0)
       (scale (0.5 0.3 0.5)
        (ref upcube)
        (rotate (0 0.8 0)
         (ref upcube))

        )
       (let ((leaf
               ' 
                 (scale (0.02 1.5 1)
                  (translate (-0.0 0.2 -0.3)
                   (rotate (0.6 0 0)
                    (ref upcube))))))
         (rgb (0.3 0.9 0.3)
                (translate (0 0.3 0)
              (for i (0.0 1.2 0.5 1.9 2.5 2.8 3.1 3.9 4.4)
                   (rotate (0.0 (bind i) 0.0)
                           (rotate (0 0 0.5)
                               (scale 0.75
                                      (bind leaf))))))

         )))))

(defvar object-3
    '(rgb (0.6 0.4 0.2)
      (translate (0 1.0 0)
       (scale 1.3
       (scale (1.3 2.5 0.1)
        (ref cube-2))))
      (rgb (0.9 0.8 0)
      (translate (-0.7 1.0 0)
       (scale (0.2 0.15 0.2)
        ;(translate (1.3 1.3 0.5)
         (ref cube-2))))

       ;)

      ))

(defvar object-4-sofa
  '(rgb (0.7 0.5 0.3)
    (translate (0 0.5 -1)
     (scale (1 1 0.2)
      (ref cube-2)))

    (translate (0 0.5 1)
     (scale (1 1 0.2)
      (ref cube-2)))
    
    (translate (-0.5 0.5 0)
     (scale (0.2 1 2)
      (ref cube-2)))
    (rgb (0.4 0.3 0.3)
     (translate (-0.0 0.5 0.5)
      (scale (1 0.5 0.9)
       (ref cube-2)
       )
      ))
    
    (rgb (0.3 0.4 0.3)
     (translate (-0.0 0.5 -0.5)
      (scale (1 0.5 0.9)
       (ref cube-2)
       )))
    (rgb (0.35 0.35 0.3)
     (translate (-0.01 0.5 -0.01)
      (scale (1 0.45 0.45)
       (ref cube-2)
       )))
    (translate (-0.3 0.65 0.0)
     (scale (0.5 0.5 0.9)
      (rgb (0.4 0.3 0.3)
     (translate (-0.0 0.5 0.5)
      (scale (1 0.5 0.9)
       (ref cube-2)
       )
      ))
      (rgb (0.3 0.4 0.3)
       (translate (-0.0 0.5 -0.5)
        (scale (1 0.5 0.9)
         (ref cube-2)
         )))
      (rgb (0.35 0.35 0.3)
       (translate (-0.01 0.5 -0.01)
        (scale (1 0.45 0.45)
         (ref cube-2)
         )))))
    ))

(defvar object-5
  '(rgb (0.75 0.5 0.3)
    (translate (0 1 0)
     (scale (1 5 1)
      (ref cube-2)))))

(defvar object-6
  '(rgb (1 0.5 0.5)
    (translate (0 1 0)
     (scale (2 2 1)
      (ref cube-2)))))

(defvar object-7
  '(rgb (0.5 1.0 0.5)
    (translate (0.25 0.5 0.25)
     (scale (0.5 2 0.5)
      (ref cube-2)))))

(defvar o8-picture
  '(bake (translate (0 2 0)
  (rgb (0.7 0.3 0.3)
    (for r (0 1 2 3)
     (rotate (0 0 (bind (* 0.5 r pi)))
      (translate (0 1 0)
       (scale (2.1 0.1 0.1)
        (ref cube-2)))))
    (rgb (0.5 0.5 0.5)
     (scale (2.0 2.0 0.02)
      (ref cube-2)))
    (translate (0 0 0.01)
     (rgb (0.5 0.5 9)
      (scale (1.0 1.0 0.02)
       (ref cube-2)))
     (translate (0 0 0.02)
      (rgb (0.5 0.9 9)
       (scale (0.1 1.5 0.02)
        (ref z-square)))

      (translate (0.2 0 0.02)
       (rgb (0.5 0.9 9)
        
       (scale (0.1 1.5 0.02)
        (ref z-square)))
      (translate (0.2 0 0.02)
       (rgb (0.5 0.9 9)
        
       (scale (0.1 1.5 0.02)
        (ref z-square)))
     ))))))))

(defvar o9-wall
  '(translate (0 0 0)
    (rgb (0.6 0.5 0.3)
     (scale (10 4 0.1)
      (translate (0.5 0 0)
       (ref upcube))))))

(defvar o10-wall
  '(translate (0 0 0)
    (rgb (0.6 0.5 0.3)
     (scale (4 4 0.1)
      (translate (0.5 0 0)
       (ref upcube))))))

(defvar o11-wall
  '(translate (0 0 0)
    (rgb (0.6 0.5 0.3)
     (scale (1 4 0.1)
      (translate (0.5 0 0)
       (ref upcube))))))

(defvar o12-picture
  '(bake
    (translate (0 2 0)
    (scale 1.1
  (rgb (0.4 0.4 0.7)
    (for r (0 1 2 3) ;; frame
     (rotate (0 0 (bind (* 0.5 r pi)))
      (translate (0 1 0)
       (scale (2.1 0.1 0.1)
        (ref cube-2)))))
   
    (rgb (0.5 0.5 0.5)
     (scale (2.0 2.0 0.02)
      (ref cube-2))) ;; canvas
   (translate (0 0 0.1)
   (for r (1 2 3 4 5 6 7 8 9)
    (scale 0.9
    (translate ((bind (- (* 0.15 r) 0.6))
                (bind (- (* 0.15 r) 0.6))
                (bind (+ 0.01 (* (sin (* 0.1 r)) 0.01))))
       (rgb ((bind (+ 0.5 (* 0.5 (sin r)))) 0.2 0.2)
        (rotate (0 0 (bind r))
                 (scale (0.15 1.2 0.2)
                  (ref z-square)))
        ))))))))))

(defvar o13-cup
  '(translate (0 0.25 0)
  (scale 0.25 (rgb (0.9 0.9 0.9)
    
    (for i (0 1 2 3)
     (rotate (0 (bind (* 0.5 pi i)))
      (translate (0.5 0 0)
       (scale (0.1 1 1)
      
       (ref cube-2)))))
   (translate (0.8 0.4 0)
    (translate (0 -0.8 0)
     (scale (0.6 0.2 0.1)
     (ref cube-2)))
    (scale (0.6 0.2 0.1)
     (ref cube-2))
    (translate (0.2 -0.4 0)
     (scale (0.2 0.9 0.1)
      (ref cube-2)))

    )
   
   (rgb (0.3 0.1 0.1)
    (translate (0 -0.0 0)
     (scale (1 0.5 1)
     (ref cube-2)
     )
    ))))))

(defvar o14-table
  '(rgb (0.7 0.4 0.2)
    (translate (0 1 0)
     (scale (3 0.1 1.5)
      (ref cube-model))
     )
    (let ((leg '(scale (0.1 1 0.1)
                 (ref cube-model))))
      (translate (0 0 0)
                 
                 (bind leg))
      (translate (0 0 1.5)
                 (scale (1 1 -1)
                        (bind leg)))
      (translate (3 0 1.5)
                 (scale (-1 1 -1)
                        (bind leg)))
      (translate (3 0 0)
                 (scale (-1 1 1)
                 (bind leg)))

    )))

(defvar o15-baby
  '(rgb (1.0 0.8 0.7)
    (scale 0.4
    (translate (0 0.5 0)
    (bind cube-2)
    (rgb (1 1 1)

     (translate (0.25 0.5 0)
      (scale (0.1 0.1 0.3)
       (bind cube-2)))
     (translate (-0.25 0.5 0)
      (scale (0.1 0.1 0.3)
       (bind cube-2))))
    
    (translate (0 0 0.8)
     (scale (0.8 0.8 0.8)
      (bind cube-2))
     (let ((arm '(scale (0.5 0.3 0.3)
                  (translate (-0.5 0 0)
                   (ref cube-2)))))
       (translate (-0.4 0 0)
                  (bind arm)
                  )
       (translate (0.4 0 0)
                  (scale (-1 1 1)
                  (bind arm)
                  ))

       )
     
     (translate (0 0 0.4)
      (scale (0.9 0.81 0.5)
       (rgb (1 1 1)
        (bind cube-2))))
     (let ((leg
             '(bake
               (translate (0.5 0 0.6)
               
               (rotate (0 0.9 0)
                (scale (0.6 0.4 0.4)
                 (translate (0.5 0 0.1)
                  (bind cube-2)))
                (translate (0.7 0 0.1)
                 (rotate (0 0.5 0)
                  (scale (0.5 0.35 0.35)
                  (bind cube-2)))))))))

       (rotate ((bind (+ -0.5 (* 0.2 (sin (* 10 real-time))))) 0 0)
               (bind leg))
       (rotate ((bind (+ -0.5 (* 0.2 (sin (+ 3.14 (* 10 real-time)))))) 0 0)
       (scale (-1 1 1)
              (bind leg))

       ))
     )
    ))))

(defvar o16-dog
  `(rgb (0.4 0.4 0.4)
    (scale 0.4
     (translate (0 2.2 0)
     (body
       (translate (-0.2 -1 0)
        (rotate (0 0 -0.5)
        (scale (1 2 1)
         (ref cube-2))))
       (translate (0.1 -1 0.5)
        (scale (0.4 1.5 0.2)
         (ref cube-2)))
       (translate (0.1 -1 -0.5)
        (scale (0.4 1.5 0.2)
         (ref cube-2)))
       (translate (-0.8 -1.5 -0.0)
        (rotate ((bind (* 0.5 (sin (* 8.0 real-time)))) 0 0)
         (rotate (0 0.0 0.7)
         (scale (0.4 1.5 0.2)
          
          (ref upcube)))))
       (let ((foot 
               '(translate (1.0 -1 0.5)
                 (translate (0 0 -0.5)
                  (scale (0.2 0.1 0.2)
                   (ref cube-2)))
                 (translate (0.2 0 -0.5)
                  (scale (0.2 0.1 0.2)
                   (rgb (0 0 0)
                    (ref cube-2)))))))
         (translate (-0.9 -0.7 -0.25)
         (bind foot)
         (translate (0 0 0.5)
                    (bind foot)))
         )
      

       )

      (head 
       (rotate (0.0 0 0.2)
      (translate (0 0.1 0)
       (scale (1 0.7 1.0)
        (ref cube-2)))
       (rgb (0.1 0 0)
       (translate (0 0.12 0.5)
        (scale (0.4 0.7 0.1)
         (ref cube-2)))
       (translate (0 0.17 -0.5)
        (scale (0.4 0.6 0.1)
         (ref cube-2))))

       
     (translate (0.75 0 0)
      (scale 0.5
       (ref cube-2))
      
      (translate (0.3 -0.3 0)
       (rgb (1 0.3 0.3)
        (scale (0.05 0.4 0.2)
        (ref cube-2))
        ))
      (translate (0.3 0.2 0)
       (rgb (0.2 0.2 0.2)
        (scale (0.05 0.2 0.2)
         (ref cube-2))
        ))
      
      )

      
     (rgb (1 1 1) ;; eyes
      (translate (0.5 0.3 0.25)
       (scale (0.01 0.3 0.15)
       
        (ref cube-2)))
      (translate (0.5 0.3 -0.25)
       (scale (0.01 0.3 0.15)
       
        (ref cube-2)))

      )))))))

(defvar obj-window
  `(translate (0 1.25 0)
    (scale 1
     (rotate ( 0 0 0)
     (rotate (,(* 0.5 pi) 0 0)
     (rgb (0.4 0.4 0.4)
    
                 (for i (0 1 2 3)
                  (rotate (0 (bind (* 0.5 pi i)))
                   (translate (0.5 0 0)
                    (scale (0.05 0.05 1)
                     
                     (ref cube-2)))))))))))


(defvar drop-point
  '(win
    (ref obj-window)))



(define model
    '(view :perspective (1.0 (bind foxgl:aspect-ratio) 0.1 1000.0)
      (depth
       (view :orthographic (1.0 1.0 2.0) 
        (translate (0.8 -0.7 0.5)
         (rotate (0 0.0 0)
          (rotate (0.9 0 0)
           (rotate (0 (bind real-time) 0)
            (scale (0.2 0.2 0.2)
             (for i (bind (mapi goal-objects (lambda (i x) (cons i x))))
              (translate (0 (bind (* (car i) 1.5)) 0)
               (bind (eval (cdr i))))
              ))))))
        (translate (-0.8 0.8 2.0)
         (scale 0.1
          (rgb (0.5 0.0 0.5)
           (translate (-1 -1 -0.2)
            (scale 2
             (ref square-model))))
          (rotate (0 0 (bind (* 3.14 2 (delta-time))))
           (translate (0 0.5 0)
            
            (scale (0.05 1.0 0.1)
             (ref cube-2)
             ))))


         ))
       
       (translate (0 -1  (bind (+ -10 zoom)))
        (rotate (0.9 0 0)
         (translate ((bind (* -1.0 player-x)) 0 (bind (* -1.0 player-y)))
          (rgb (0.7 0.7 0.5)
           (for f (bind floor-tiles)
            (let ((p (car f)) (s (cadr f)))
              (translate ((bind (car p)) 0.5 (bind (cadr p)))
                  (scale ((bind (car s)) 1 (bind (cadr s)))
                                (ref tile-model-2)
              )))))
         
         (rgb (0 0 0)
          (translate ((bind player-x) 0 (bind player-y))
           (rotate (0 (bind ang) 0)
            (ref model-player)
            (translate (0 1.3 0)
            
            (rotate ((bind player-body-rot) 0 0)

             (translate (0 1.5 0)
              (for p (bind (mapi picked-objects (lambda (i x) (cons i x))))
               (translate (0 (bind (* (car p) 1.1)) 0)
                (bind (eval (cadddr p))))
               )))))))
          
          (for obj (bind level-objects)
           (translate (bind (car obj))
            (rotate (0 (bind (cadr obj)) 0)
             (bind (eval (caddr obj)))
             )))
          (translate (0 0.0 5)
           (scale (1 1 1)
            (ref drop-point)))
         ))))))
        

