(load "lisp1.lisp")
(load "foxgl.lisp")
;(thread-start swank:start-server)

(define square-model '(polygon :2d-triangle-strip (0 0 1 0 0 1 1 1)))

(define cube-model '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 1 0
                                                 1 1 0
                                                 0 1 1
                                                 1 1 1
                                                 0 0 1
                                                 1 0 1
                                                 0 0 0
                                                 1 0 0
                                                 ;; side 1
                                                 1 0 0
                                                 1 0 1
                                                 1 1 0
                                                 1 1 1
                                                 )))

(define tile-model '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 0 1
                                                 1 0 1)))



(define wheel-model
    `(color :rgb (0.1 0.1 0.1)
      (transform :scale (0.5 0.5 0.8)
       ,cube-model)))

(define car-model
    `(car
      (transform :translate (-1 0 -2)
      (blend
       (transform :scale (2 1 4) :translate (-0.2 -0.5)
        (color :rgb ( 0.2 0.2 0.2)
         
         ,tile-model)))
       
       (transform :translate (0 -0.6 0) ,wheel-model)
      (transform :translate (1.5 -0.6 0) ,wheel-model)
      (transform :translate (0 -0.6 3) ,wheel-model)
      (transform :translate (1.5 -0.6 3) ,wheel-model)


      (color :rgb (1 0 0)
       (transform :scale (2 1 4)
        ,cube-model))
      
      (color :rgb (0 0 0)
       (transform :translate (0.25 1.0 0.5)
        :scale (1.5 0.8 2.0)
        ,cube-model))

       ;; sun roof
       (color :rgb (1 0 0)
        (transform :translate (0.25 2.0 0.5)
                   :scale (1.5 0.8 2.0)
         ,tile-model)
        )
       (color :rgb (0 0 0)
        (transform :translate (0.4 2.0 1.2)
                   :scale (1.2 0.8 0.9)
         ,tile-model)
        )
       )))

(define tree-model
    `(color :rgb (0.5 0.8 0.5)
      (transform :scale (2 1 2)
                                        ;,square-model
       (transform :translate (0 4 0)
        (polygon :2d-triangle-strip
         (0 0
          -0.5 -1
          0.5 -1
          0 -0.5
          -0.7 -2
          0.7 -2
          0 -1
          -1.0 -3
          1.0 -3
          ))))
  
  (color :rgb (0.9 0.7 0.4)
         (transform :scale (1 1 1) :translate (-0.5 0.0)
                    ,square-model))
      ))

(defun world-model ()
  `(world
    (transform :id world-base :translate (0 0 0)
     (color :rgb (0.3 0.3 0.3)
      (transform :scale (50 1 10) :translate (-15.0 0.0 -1.0) ,tile-model))
                                        ;,square-model
      (transform :translate (-2 0 -6)
       (ref tree-model))
      (transform :translate (2 0 -6)
       (ref tree-model))
      (transform :translate (-2 0 -2)
       (ref tree-model))
      (transform :translate (-4 0 -2.2)
       (ref tree-model))
      (transform :translate (4.3 0 -3)
       (ref tree-model))
     
     (transform :id car :translate (0 0 0) :rotate (0.0 1.5 0.0)
      (ref car-model))
     ))
  )

(define heart-model
  `(heart
    (color :rgb (1 0 0)
     (transform :scale( 0.5 0.7) :translate (0 0.5)
     (polygon :2d-triangle-strip
      (-0.75 0.0
       -0.25 0.0
       -1 -0.25
       0 -0.25
       -1 -0.5
       0 -1.5))
     (polygon :2d-triangle-strip
      (0.75 0.0
       0.25 0.0
       1 -0.25
       0 -0.25
       1 -0.5
       0 -1.5))
     ))))

(define gas-model
    `(gas
      (transform :translate (0 -0.5)
      ; background
      (color :rgb (0.8 0.8 0.0)
       (transform :scale( 0.5 0.7) :translate (0 0.5)
        (polygon :2d-triangle-strip
         (-0.25 1.0
          0.25 1.0
          -1 0.5
          1 0.5
          -1 -0
          1 -0
          -0.25 -0.5
          0.25 -0.5
          ))))
      (transform :scale (0.5 0.5) :translate (0 0.6)
      (color :rgb (0 0 0)
       (polygon :2d-triangle-strip
        (0 1
         -0.4 0
         0.4 0
        -0.4 -0.2
        0.4 -0.2
        -0.2 -0.4
        0.2 -0.4
        ))
      )))))
(define dial-model
    `(dial
      (transform :translate (0 -0.7)
      ; background
      (color :rgb (0.3 0.3 0.3)
       (transform :scale( 0.5 0.7) :translate (0 0.5)
        (polygon :2d-triangle-strip
         (-0.25 1.0
          0.25 1.0
          -1 0.5
          1 0.5
          -1 -0
          1 -0
          -0.25 -0.5
          0.25 -0.5
          ))))
      (transform :scale (0.5 0.5) :translate (0 0.7)
      (color :rgb (0.2 0.2 0.2)
       (polygon :2d-triangle-strip
        (0 1
         -0.4 0.2
         0.4 0.2
        ))
      )))))

(define dollar-model
    `(dollar
      (transform :scale (0.5 0.5) :translate (-0.2 -0.3)
      (transform :translate (0.1 -0.5) :rotate (0.0 0.0 1.0)
              (transform :scale (2 1)
               (color :rgb (0.15 0.65 0.15)
                ,square-model)))

      (transform :translate (0.3 -0.5) :rotate (0.0 0.0 1.0)
              (transform :scale (2 1)
               (color :rgb (0.2 0.7 0.2)
                ,square-model)))

      (transform :translate (0.5 -0.5) :rotate (0.0 0.0 1.0)
              (transform :scale (2 1)
               (color :rgb (0.3 0.8 0.3)
                ,square-model))
       (transform :translate (0.7 -0.3)
        (transform :translate (-0 0) :scale (0.05 0.05) (text "$") ))
       ;(color :rgb (0.1 0.4 0.1)
       ; (transform :scale (1.5 -2.3)
       ;  (transform :translate (0.0 -1.05)
       ;          (flat :size (128 128)
       ;           (transform :scale (2 2)
       ;            (text "$"))))))

       ))))
(define therm-base '(polygon :2d-triangle-strip
       (0 3
        0.3 3
        0 0
        0.3 0
        -0.3 0.3
        0.6 0.3
        -0.3 0.6
        0.6 0.6
        0.0 0.9
        0.3 0.9
        )))
(define thermometer-model
    `(thermo
      (transform :scale (0.3 0.3) :translate (0 -0.3)
       (ref therm-base)
       (transform :scale (0.5 0.5) :translate (0.06 0.2)
        (color :rgb (0 0 1)
         (ref therm-base)))
       )))
(define trinagle '(polygon :2d-triangle-strip (0 0 0.5 1 1 0)))
(define happy-fox-model
    `(happy-fox
      (transform :scale (0.5 0.5) :translate (-0.5 0.25)
       
       (color :rgb (0.9 0.5 0.2)
        (polygon :2d-triangle-strip (0 0 1 -1 2 0))
        (polygon :2d-triangle-strip (0 0 0.2 0.4 0.4 0 ))
        (transform :translate (1.6 0) :scale (0.4 0.4) ,trinagle)
        (color :rgb (0 0 0)
         (transform :scale (0.4 0.4) :translate (0.5 -0.3)
          ,trinagle))
        (color :rgb (0 0 0)
         (transform :scale (0.4 0.4) :translate (1.2 -0.3)
          ,trinagle))
        ))))

(define bg-color '( 0.15 0.3 0.1))
(define time 0.0)
(define mx 0.0)
(define vx 0.0)
(define world (world-model))
(defun model-find-elem(obj id)
  
  (let ((result nil)
        (model (cdr obj)))
    (loop (and model (not result))
          (cond
            ((list? (car model))
             (progn
               (set result (model-find-elem (car model) id))
               (set! model (cdr model))))
            ((symbol? (car model))
             (progn

               (when (and (eq (car model) :id)
                          (eq (cadr model) id))
                 (set! result obj))
               (set! model (cddr model))))
            (else
             (set! model (println (cdr model))))))
    result))
(define car-object (model-find-elem world 'car))
(define world-base (model-find-elem world 'world-base))
(assert car-object)
(assert world-base)

(defun pset! (list key value)
  (loop list
        (when (eq (car list) key)
          (set! list (cdr list))
          (set-car! list value)
          (set! list nil))
        (set! list (cdr list))
        ))
(define test-obj '(car :a 2 :b 2))
(pset! test-obj :a 3)
(println test-obj)
(println car-object)
(pset! car-object :rotate '(0.0 -0.5 0.0))
(define car-rotation 0.0);
(define car-position (make-vector 3 (float32 0.0)))
(define forward (let ((v (make-vector 3 (float32 0.0))))
                  (vector-set! v 2 (float32 1.0))
                  v))
(define scale-down (mat4-scale 0.1 0.1 0.1))
(defun mat:add-inplace(v1 v2)
  (do-times (vector-length v1)
    (lambda (i) (vector-set!  v1 i (float32  (+ (rational (vector-ref v1 i)) (rational (vector-ref v2 i)))))))
  v1)
(defun render-scene()
  
  (incf time 0.1)
  (incf mx vx)
  (set! vx (* vx 0.9))
  (pset! car-object :rotate `( 0.0 ,car-rotation 0.0))
  (pset! car-object :translate `(,(vector-ref car-position 0) 0.0 ,(vector-ref car-position 2)))

  (pset! world-base :translate `(,(- 0.0 (vector-ref car-position 0)) 0.0 ,(- 0.0 (vector-ref car-position 2))))
  
  (let ((car-direction (mat4:rotate 0.0 car-rotation 0.0)))
    (let ((vd (mat-mul (mat4-scale vx vx vx) (mat-mul car-direction forward))))
                                        ;      (println vd)
      (mat:add-inplace car-position vd)
;      (println car-position)
      
    
    
    ))
  (render-model
   `(root
     (transform :scale (2.0 2.0)
      (transform :translate (-0.5 -0.5)
       
       (color :rgb ,bg-color
        ,square-model)

       ;; setting up the view
       (view :perspective (1.0 1.0 0.01 100.0)
        (transform :translate (0 -4 -4)
         (transform :rotate (0.4 0.0 0)
          (transform :translate (-0.5 -0.5 -10)
           :scale (0.5 0.5 0.5)
           
         
           ,world ))))
       (transform :scale (0.05 0.05) :translate (0.05 0.95)
        (ref heart-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        (transform :scale (,(+ 1.0 (sin (+ 0.5 time))) 0.3) :translate (0.8 0)
         (color :rgb (1 0 0)
          (ref square-model)))
        )
       (transform :scale (0.05 0.05) :translate (0.05 0.85)
        (ref gas-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        (transform :scale (,(+ 1.0 (sin time)) 0.3) :translate (0.8 0)
         (color :rgb (0 0 0)
          (ref square-model)))
        )
       (transform :scale (0.05 0.05) :translate (0.05 0.75)
        (ref thermometer-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        
        (transform :scale (,(+ 1.0 (sin time)) 0.3) :translate (0.8 0)
         (color :rgb (,(+ 0.5 (* 0.5 (sin (+ 0.0 time))))
                      0
                      ,(+ 0.5 (* 0.5 (sin (+ 1.5 time))))
                      )
          (ref square-model)))
        )
       (transform :scale (0.05 0.05) :translate (0.05 0.65)
        (ref happy-fox-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        
        (transform :scale (,(+ 1.0 (sin time)) 0.3) :translate (0.8 0)
         (color :rgb (0.9 0.5 0.2) 
          (ref square-model)))
        )
       
       (transform :scale (0.05 0.05) :translate (0.05 0.45)
        (ref dollar-model)
        )
       (transform :scale (0.1 0.1) :translate (0.1 0.1)
        (transform :rotate (0 0 ,time)
        (ref dial-model)
        
         ))
       (transform :scale (0.1 0.1) :translate (0.3 0.1)
               (transform :rotate (0 0 ,(- 0.0 time))
        (ref dial-model)
        
                ))
       (transform :scale (0.1 0.1) :translate (0.6 0.1)
        (transform :rotate (0 0 ,(- 0.0 time))
         (ref dial-model)))
       
       )
    
      ))))
(defun poll-events2 ()
  (let ((evts (foxgl-get-events)))
    (let ((evts2 (map evts (lambda (evt)
                             (let* ((tsl (cddr evt))
                                    (ts (car tsl))
                                    (tsl (cdr tsl))
                                    (e (car tsl)))
                               (cons e (cdr tsl)))))))
      evts2
      )))
(defun game-update ()
  ;(sleep 0.1)
  (let ((evts (poll-events2)))
    (when evts
      (println evts)))
  (when nil
    (do-times 320 (lambda (i) (when (> i 31)
                                (when (foxgl:key-down? win i)
                                  (println (list 'yes i)))))))
  (when (foxgl:key-down? win foxgl:key-w)
    (set! vx (+ vx 0.1))
    )
  (when (foxgl:key-down? win foxgl:key-s)
    (set! vx (- vx 0.1))
    )
  (when (foxgl:key-down? win foxgl:key-a)
    (set! car-rotation (- car-rotation 0.1))
    )
  (when (foxgl:key-down? win foxgl:key-d)
    (set! car-rotation (+ car-rotation 0.1))
    )
  (audio:update)
  (measure
   (do-times 100 (lambda ()
                  (render-scene))))
  (swap win)
  (poll-events)
  )

(define win (create-window (integer 700) (integer 700)))
(set-title win "Drive My Car")
(make-current win)
(load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))

(loop t
     (thread:lock-mutex *swank-mutex*)
     (with-exception-handler
         (game-update)
       (lambda (x)
         (println x)
         (sleep 0.5)
         ))
     (thread:unlock-mutex *swank-mutex*)
     ;(lisp:exit)
     )

