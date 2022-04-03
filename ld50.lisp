(load "lisp1.lisp")
(load "foxgl.lisp")
(load "vec2.lisp")
(thread-start swank:start-server)

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
                                                 ;; step to side 2
                                                 0 1 1
                                                 ;; side 2
                                                 0 1 1
                                                 0 0 1
                                                 0 1 0
                                                 0 0 0
                                                 
                                                 )))

(define tile-model '(polygon :3d-triangle-strip (0 0 0
                                                 1 0 0
                                                 0 0 1
                                                 1 0 1)))



(define wheel-model
    `(color :rgb (0.1 0.1 0.1)
      (transform :scale (0.5 0.5 0.8)
       (ref cube-model))))

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
        (ref cube-model)))
      
      (color :rgb (0 0 0)
       (transform :translate (0.25 1.0 0.5)
        :scale (1.5 0.8 2.0)
        (ref cube-model)))

       ;; sun roof
       (color :rgb (1 0 0)
        (transform :translate (0.25 1.9 0.5)
                   :scale (1.5 0.8 2.0)
         ,tile-model)
        )
       (color :rgb (0 0 0)
        (transform :translate (0.4 2.002 1.2)
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

(define test-bezier '(0.0 0.0
                      1.0 1.0
                      2.0 1.5
                      3.0 2.0
                      3.0 3.0 ))

(defun bezier-to-polygon (curve)
  (let ((points nil))
    (loop (cddr curve)
          (let ((ax (pop curve))
                (ay (pop curve))
                (bx (pop curve))
                (by (pop curve))
                (cx (car curve))
                (cy (cadr curve))
                )
            (do-times 30
              (lambda (i)
                (let* ((d (/ (rational i) 30.0))
                       (dn (- 1.0 d))
                       (dn2 (* dn dn))
                       (d2 (* d d)))
                  (let ((px (+ bx (+ (* dn2 (- ax bx)) (* d2 (- cx bx)))))
                        (py (+ by (+ (* dn2 (- ay by)) (* d2 (- cy by))))))
                    (set! points (cons py (cons px points)))))))))
    (let ((cx (pop curve))
          (cy (pop curve)))
      (set! points (cons cy (cons cx points))))
    (reverse! points)))


(defun v2-len(x y)
  (math:sqrt (+ (* x x) (* y y))))

(defun points-to-strip (points)
  (let ((strip nil))
    (loop (cddr points)
          (let ((ax (pop points))
                (ay (pop points))
                (bx (car points))
                (by (cadr points)))
            (let ((dx (- bx ax))
                  (dy (- by ay)))
              (let ((len (v2-len dx dy)))
                (set! dx (/ dx len))
                (set! dy (/ dy len)))
              (let ((ty (* dx 0.25))
                    (tx (* (- 0.0 dy) 0.25)))
                (let ((nx1 (+ ax tx))
                      (ny1 (+ ay ty))
                      (nx2 (- ax tx))
                      (ny2 (- ay ty)))
                  (set! strip (list* ny2 nx2 ny1 nx1 strip)))))))
    (println strip)
    (reverse! strip)))
(println (list-to-array '(1 2 3)))
(defun extrude-2d-path (strip thickness)
  (let ((out nil)
        (len (/ (length strip) 2))
        (strip2 (list-to-array strip))
        (z (- 0.0 thickness)))
    (do-times len
      (lambda (x)
        (let ((i2 (* x 2))
              (x (vector-ref strip2 i2))
              (y (vector-ref strip2 (+ i2 1))))
          (set! out (list* z y x 0.0 y x out)))))
    (reverse! out)
    ))

(println (extrude-2d-path '(0.0 0.0
                   1.0 0.0
                   1.0 1.0
                   0.0 1.0)
                 1.0))
                 
(println (bezier-to-polygon test-bezier))
(println (points-to-strip (bezier-to-polygon test-bezier)))

(define coin-model
    `(coin
      (transform :rotate (0 1.0 0)
       (color :rgb (1 1 0)
        (color :rgb (0.4 0.4 0.35)
         (transform :translate (-0.95 -0.7 -0.35)
          :scale (1.9 1.0 0.3)
          
          ,tile-model))
      (polygon :2d-triangle-strip
       (-0.3 1.0
        0.3 1.0
        -1 0.5
        1 0.5
        -1 -0
        1 -0
        -0.25 -0.5
        0.25 -0.5
        ))
       (polygon :3d-triangle-strip
        ,(println (extrude-2d-path '(-0.3 1.0
                            0.3 1.0
                            1.0 0.5
                            1.0 0.0
                            0.25 -0.5
                            -0.25 -0.5
                            -1.0 0.0
                                     -1.0 0.5
                                     -0.3 1.0)
                   0.3)))

       ))))

(define road-line ())
(define road-model
    `(transform :scale (20 20 20)))
(define start-road-location 0)
(let ((points '(0.0 0.0
                  2.0 1.0
                3.0 3.0
                  6.0 7.0
                9.0 0.0
                  12.0 1.0
                13.0 2.0
                15.0 0.0
                16.0 0.0
                18.0 0.0
                20.0 0.0
                25.0 1.0
                30.0 0.0
                33.0 -3.0
                40.0 -5.0
                45.0 -10.0
                45.0 -20.0
                50.0 -25.0
                55.0 -20.0
                
                
                
     )))
  (set! road-line (bezier-to-polygon points))
  
  (set! road-model
    `(transform :scale (20 20 20)
      (transform :rotate (,pi_2 0 0)
       (polygon :2d-triangle-strip ,(points-to-strip road-line)))))
  )

(println road-model)
(println road-line)
(define world-model
  `(world
    (transform :id world-base :translate (0 0 0)
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

     (transform :translate (493.3 0 0)
      (ref tree-model))
     (transform :translate (612.3 0 -17)
      (ref tree-model))

     (transform :translate (0 0 3)
      (color :rgb (0.3 0.3 0.3)
       (ref road-model))
      )
     (depth     
     (transform :id car :translate (0 0 0) :rotate (0.0 1.5 0.0)
      (ref car-model))     

      (transform :translate (49.7 0.0 45) :id coin
                 :rotate (0 0 0)
       (ref coin-model)))

       

     (transform :translate (351.3 0 12)
      (ref tree-model))
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
(define bg-color '( 0.9 0.9 0.9))
(define bg-color '( 0.9 0.9 0.6))
(define time 0.0)
(define vx 0.0)
;(define world (world-model))
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

;(pset! car-object :rotate '(0.0 -0.5 0.0))
(define car-rotation 0.0);
(define car-offset 0.0)
(define car-y-offset 0.0)
(define car-road-location 0)
(define gas-level 100.0)
(define health-level 100.0)
(define temperature 7.0)
(define ambient-temperature 100.0)
(define ac-state :heat)
(define radio-state :off)
(define fox-happiness 100.0)
(define traveled 0.0)
(define end-state nil)
(progn
  (set! car-road-location 22)
                                        ; car-y-offset car-offset
  (set! car-y-offset -3.9)
  (set! car-offset 1.3)
  (set! vx 0.0)
  )
(define car-turn 0.0)
(define forward (let ((v (make-vector 3 (float32 0.0))))
                  (vector-set! v 2 (float32 1.0))
                  v))

(defun list-offset(lst i)
  (do-times i (lambda (x) (pop lst)))
  lst)
    
(define scale-down (mat4-scale 0.1 0.1 0.1))
(defun mat:add-inplace(v1 v2)
  (do-times (vector-length v1)
    (lambda (i) (vector-set!  v1 i (float32  (+ (rational (vector-ref v1 i)) (rational (vector-ref v2 i)))))))
  v1)

(defun abs(x)
  (if (< x 0.0)
      (- 0 x)
      x))

(defun render-scene()
  
  
  (render-model
   `(root
     (transform :scale (2.0 2.0)
      (transform :translate (-0.5 -0.5)
       
       (color :rgb ,bg-color
        ,square-model)

       ;; setting up the view
       (view :perspective (1.0 1.0 0.01 1000.0)
        (transform :translate (0 -4 -4)
         (transform :rotate (0.4 0.0 0)
          (transform :translate (-0.5 -0.5 -10)
           :scale (0.5 0.5 0.5)
           
           
           (ref world-model) ))))
       ;health meter
       (transform :scale (0.05 0.05) :translate (0.05 0.95)
        (ref heart-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        (transform :scale (,(/ health-level 50.0) 0.3) :translate (0.8 0)
         (color :rgb (1 0 0)
          (ref square-model)))
        )
       ;; gas
       (transform :scale (0.05 0.05) :translate (0.05 0.85)
        (ref gas-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        (transform :scale (,(/ gas-level 50.0 ) 0.3) :translate (0.8 0)
         (color :rgb (0 0 0)
          (ref square-model)))
        )
       ;;thermometer
       (transform :scale (0.05 0.05) :translate (0.05 0.75)
        (ref thermometer-model)
        (transform :scale (2 0.3) :translate (0.8 0) (ref square-model))
        
        (transform :scale (,(/ temperature 50.0) 0.3) :translate (0.8 0)
         (color :rgb (,(+ 0.5 (- (* 0.5 (/ temperature 50.0)) 0.5))
                      ,(- 1 (abs (- (* 0.5 (/ temperature 50.0) 0.5))))
                      ,(- 0.5 (- (* 0.5 (/ temperature 50.0)) 0.5)))
                      
          (ref square-model)))
        )
       (transform :scale (0.05 0.05) :translate (0.05 0.65)
        (ref happy-fox-model)
        (transform :scale (2.0 0.3) :translate (0.8 0) (ref square-model))
        
        (transform :scale (,(/ fox-happiness 50.0) 0.3) :translate (0.8 0)
         (color :rgb (0.9 0.5 0.2) 
          (ref square-model)))
        )
       
       (transform :scale (0.05 0.05) :translate (0.05 0.55)
        (transform :scale (0.03 -0.03)
         (color :rgb (0 0 0)
         (text "traveled: ")
         (transform :translate (100.0 0.0)
          (text ,(value->string (integer traveled)))
          ))))

       (transform :scale (0.05 0.05) :translate (0.25 0.85)
        (transform :scale (0.1 -0.1)
         (color :rgb (0 0 0)
          (text ,(if end-state
                     (value->string end-state) ""))
         )))
              
       (transform :scale (0.05 0.05) :translate (0.05 0.45)
        (ref dollar-model)
        )

       (transform :scale (0.1 0.1) :translate (0.1 0.1)
        (transform :scale (10.0 2.0) :translate (-1.0 -1)
         (color :rgb (0.3 0.0 0.0)
          ,square-model))
        (transform :rotate (0 0 ,(if (eq ac-state :heat) -1.0
                                     (if (eq ac-state :cool)
                                         1.0
                                         0.0)))
         (ref dial-model)
         )
        (transform :scale (0.015 -0.015) :translate (-0.3 1)
         (text "AC"))
        )
       ;; radio
       (transform :scale (0.1 0.1) :translate (0.3 0.1)
        (transform :rotate (0 0 ,(if (eq? radio-state :off) 1.0 -1.0))
         (ref dial-model))
        (transform :scale (0.015 -0.015) :translate (-0.3 1)
         (text "Radio"))
        )
       (transform :scale (0.1 0.1) :translate (0.6 0.1)
        (transform :rotate (0 0 ,(- 0.0 time))
         (ref dial-model)))
       (transform :scale (0.1 0.1) :translate (0.0 0.0)

        (transform :scale (0.015 -0.015) :translate (-0.0 1)
         (text ,(value->string (list fox-happiness)))
        ))
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

(defun entities-update ()
  (let ((evts (poll-events2)))
    (when evts
      
      (println evts))
    (when (first (lambda (x) (equals? '(char e) x)) evts)
      (set! ac-state
            (cond ((eq ac-state :cool) :heat)
                  ((eq ac-state :heat) :off)
                  ((eq ac-state :off) :cool)
                  (else :cool))))
    (when (first (lambda (x) (equals? '(char r) x)) evts)
      (set! radio-state
            (cond ((eq radio-state :off) :on)
                  (else :off))))
    )
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
  (set! car-turn 0.0)
  (when (foxgl:key-down? win foxgl:key-a)
    (incf car-turn -0.3)
    )
  (when (foxgl:key-down? win foxgl:key-d)
    (incf car-turn  0.3)
    )
  (incf car-rotation (* 1.5 car-turn))
  (incf car-y-offset (- 0.0 car-turn))
  (incf time 0.1)
  (when (> vx 1.0)
    (set! vx 1.0))
  ;(set! vx 1.0)
                                        ;(set! vx (* vx 0.9))
  (define out-of-gas nil)
  (when (or end-state (< gas-level 0.00))
    (set! vx 0.0)
    (set! out-of-gas t)
    )
  (define road-part (list-offset road-line (* car-road-location 2)))
  (unless (or end-state out-of-gas)
    (when (eq ac-state :heat)
      (incf temperature 0.1))
    (when (eq ac-state :cool)
      (incf temperature -0.1))
    (when (not (eq ac-state :off))
      (incf gas-level -0.001))
    (when (eq radio-state :on)
      (incf gas-level -0.001))
  
    (if (and (eq radio-state :on) (< fox-happiness 100.0))
        (incf fox-happiness 0.05)
        (incf fox-happiness -0.05))

    (incf car-offset vx)
    (incf traveled vx)
    )
  
  (incf temperature (* 0.001 (- ambient-temperature temperature)))
  
  (if (< vx 0.0)
      (set! vx 0.0))
         
  (when (> vx 0.0)
    (incf gas-level -0.01)
    )
  (when (< temperature 20.0)
    (incf health-level -0.1))
  (when (> temperature 80.0)
    (incf health-level -0.1))
  (when (< fox-happiness 20.0)
    (incf health-level -0.1))
  (when (> fox-happiness 80.0)
    (incf health-level 0.05))
  
  (when (< gas-level 0.001)
    (set! end-state :out-of-gas))
  (when (< health-level 0.001)
    (set! end-state :dead))

  
  (define p1 (vec2-scale (list->vec2 road-part) 20.0))
  (define car-object (model-find-elem world-model 'car))
  (define coin-object (model-find-elem world-model 'coin))

  ;(println coin-object)
  (pset! coin-object :rotate `(0.0 ,time 0.0))
  (unless (cdddr road-part)
    (set! car-offset 0.0)
    (set! car-road-location start-road-location))
  (when (cdddr road-part)
          
    (define p2 (vec2-scale (list->vec2 (list-offset road-part 2)) 20.0))
    (let* ((dv (vec2- p2 p1))
           (dvn (vec2-normalize dv))
           (dvt (vec2-90 dvn))
           (dvns (vec2-scale dvn car-offset))
           (cpos (vec2+ p1 dvns))
           
           (world-base (model-find-elem world-model 'world-base))
           )
      (when (> (vec2-len (vec2- cpos p1)) (vec2-len dv))
        (set! car-offset 0.0)
        (incf car-road-location 1)
        )
      (set! cpos (vec2+ cpos (vec2-scale dvt car-y-offset)))
      
      (let* ((car-dir (vec2-rotate (vec2 -1 0) car-rotation))
             (ang (vec2-dot car-dir dvn)))
        (set! car-rotation (+ car-rotation  (* (float32 0.5) ang)))
        ;;(println dvt)
        )
        
    
      ;(println (list p1 dvn car-offset (* (float32 3.0) (float32 4.0 ))))
    (pset! car-object :rotate `( 0.0 ,car-rotation 0.0))
    (pset! car-object :translate `(,(rational (vec2-x cpos)) 0.0 ,(rational (vec2-y cpos))))
    (pset! world-base :translate `(,(- 0.0 (vec2-x cpos)) 0.0 ,(- 0.0 (vec2-y cpos 1))))
    )))

(defun game-update ()
  ;(sleep 0.1)
  
  (audio:update)
  (foxgl:clear)
  (entities-update)
  (render-scene)
  (when nil
    (measure
     (do-times 1 (lambda ()
                  (render-scene)))))
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

