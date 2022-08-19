
(println "starting ld50 game")
(load "lisp1.lisp")
(lisp:collect-garbage)
(load "foxgl.lisp")
(lisp:collect-garbage)
(load "vec2.lisp")
(lisp:collect-garbage)
(define swank-loaded nil)
(unless (or t lisp:*web-environment*)
  (load "swank.lisp")
  (set! swank-loaded t)
  (thread:start swank:start-server)
  )
(load "models.lisp")


(define wheel-model
    '(color :rgb (0.1 0.1 0.1)
      (transform :scale (0.5 0.5 0.8)
       (ref cube-model))))

(define car-model
    '(car
      (transform :translate (-1 0 -2)
        (wheels
         (translate (0 -0.6 0) (ref wheel-model))
         (translate (1.5 -0.6 0) (ref wheel-model))
         (translate (0 -0.6 3) (ref wheel-model))
         (translate (1.5 -0.6 3) (ref wheel-model)))
       (color :rgb (1 0 0)
        (scale (2 1 4)
         (ref cube-model)))
       (color :rgb (0 0 0)
        (transform :translate (0.25 1.0 0.5)
         :scale (1.5 0.8 2.0)
         (ref cube-model)))
       ;; sun roof
       (color :rgb (1 0 0)
        (transform :translate (0.25 1.9 0.5)
         :scale (1.5 0.8 2.0)
         (ref tile-model))
        )
       (color :rgb (0 0 0)
        (transform :translate (0.4 2.002 1.2)
         :scale (1.2 0.8 0.9)
         (ref tile-model))
        )
       )))

(define tree-model
    '(color :rgb (0.5 0.8 0.5)
      (scale (2 1 2)
       (translate (0 4 0)
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
        (ref square-model))
      )))

(define ice-cube-model
    '(color :rgb (0.6 0.6 0.9)
      (ref cube-model)
      (translate (1 0 0.6)
       (color :rgb (0.7 0.7 1.0)
        (ref cube-model))
       )
      (translate (0.7 1 0.3)
       (ref cube-model)
       )
      ))

(define rocks-model
    '(color :rgb (0.4 0.4 0.4)
      (transform :scale (2.0 2.0 2.0)
       :rotate (0.0 5.0 0.0)
       (transform
        :scale (1.2 1.2 0.8)
        (ref cube-model))
       
       (transform :translate (1 0 0.6)
        :rotate (0 1.0 0.0)
        :scale (0.8 1.2 1.1)
        (color :rgb (0.4 0.4 0.4)
         (ref cube-model))
        )
       (transform :translate (0.7 1 0.3)
        :rotate (0 0.3 0.0)
        :scale (1.2 1.3 1.1)
        (ref cube-model)
        ))))


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
         (do-times 20
           (lambda (i)
             (let* ((d (/ (rational i) 20.0))
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
      (rotate (0 1.0 0)
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

(define gascan-model
    '(translate (-0.5 0.0 -0.5)
      (color :rgb (0.8 0 0)
       (ref cube-model))
      (transform :translate (0 0.8 0.5) :scale (0.2 0.4 0.2) :rotate (0 0 0.3)
       (color :rgb (0 0 0)
        (transform :translate (0 0.5 0) :rotate (0 0 0.5)
         (ref cube-model))
        (ref cube-model))
       )))

(define road-line ())
(define road-length 0.0)
(define road-model
    `(transform :scale (20 20 20)))
(define start-road-location 0)
(defun lines-length(points)
  (let ((length 0.0))
    (loop (cddr points)
       (let ((ax (pop points))
             (ay (pop points))
             (bx (car points))
             (by (cadr points)))
         (incf length (vec2:len (vec2 (- ax bx) (- ay by))))))
    length))

(defun follow-lines (points len)
  (let ((length 0.0))
    (loop (and (cddr points) (< length len))
       (let ((ax (pop points))
             (ay (pop points))
             (bx (car points))
             (by (cadr points)))
         (incf length (vec2:len (vec2 (- ax bx) (- ay by))))))
    (cddr points)))



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
                -10.0 -35.0
                30.0 -30.0
                20.0 -35.0
                40.0 -40.0
                20.0 -45.0
                30.0 -50.0
                20.0 -55.0
                40.0 -60.0
                20.0 -65.0
                30.0 -70.0
                20.0 -80.0
                10.0 -70.0
                -10.0 -75.0
                -20.0  -60.0

                )))
  (set! road-line (bezier-to-polygon points))
  
  (set! road-model
        `(transform :scale (20 20 20)
          (transform :rotate (,pi_2 0 0)
           (polygon :2d-triangle-strip ,(points-to-strip road-line)))))
  (set! road-length (* 20.0 (lines-length road-line)))
  )


(println road-model)
(println road-line)
(define world-model
    `(world
      (transform :id world-base :translate (0 0 0)

       (depth     

        (background :id background
         (scope :id trees
          
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -6)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -60)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform :id tree :translate (-2 0 -600)
           (ref tree-model))
          (transform  :id tree :translate (2 0 -6)
           (ref tree-model))
          (transform  :id tree :translate (-2 0 -2)
           (ref tree-model))
          (transform  :id tree :translate (-4 0 -2.2)
           (ref tree-model))
          (transform  :id tree :translate (4.3 0 -3)
           (ref tree-model))
          (transform  :id tree :translate (493.3 0 0)
           (ref tree-model))
          (transform  :id tree :translate (612.3 0 -17)
           (ref tree-model))
          (transform :id tree :translate (351.3 0 12)
           (ref tree-model))
          )
         
         (transform :id gas :translate (10 0 10) :rotate (0 0 0)
          (ref gascan-model)
          )
         )
        (transform :translate (0 -0.55 0)
         (color :rgb (0.3 0.3 0.3)
          (ref road-model))
         )
        (measure-model
          (transform :id car :translate (0 0 0) :rotate (0.0 0.0 0.0)
           (ref car-model)))

        (transform :translate (49.7 0.0 45) :id coin
         :rotate (0 0 0)
         (ref coin-model))

        (transform :id ice-cube :translate (10.0 0.0 0.0) :id coin
         :rotate (0 0 0)
         (ref ice-cube-model))

        (transform :id rocks :translate (13.0 0.0 0.0) :id coin
         :rotate (0 0 0)
         (ref rocks-model))
        ))))

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

(define normal-color '( 0.15 0.3 0.1))
(define cold-color '(0.9 0.9 0.9))
(define hot-color '(0.9 0.9 0.6))

(define bg-color '( 0.15 0.3 0.1))
(define time 0.0)
(define vx 0.0)

(println (concat '(1 2 3) '(4 5 6) '(7 8 9)))

(defun detect-collision (obj1 obj2 threshold)

  (let ((p1 (list->vec3 (plookup (cdr obj1) :translate)))
        (d1 (list->vec3 (plookup (cdr obj1) :rotate)))
        (p2 (list->vec3 (plookup (cdr obj2) :translate))))
    (let ((p11 (vec3+ p1 (math:* (vec3-rotation d1) (vec3 0 0 0.9)))))
      (let ((d (vec3-len (vec3- p11 p2))))
        (when (< d (or threshold 1.5)) :collision)))))

(defun model-find-elem(obj id)
  
  (let ((result nil)
        (scope (eq (car obj) 'scope))
        (model (cdr obj)))
    (loop (and model (not result))
       (cond
         ((list? (car model))
          (unless scope
            (set result (model-find-elem (car model) id)))
          (set! model (cdr model)))
         ((symbol? (car model))

          (when (and (eq (car model) :id)
                     (eq (cadr model) id))
            (set! result obj))
          (set! model (cddr model)))
         (else
          (set! model (cdr model)))))
    result))

(defun model-find-elems(obj id)
  (let ((result nil)
        (scope (eq (car obj) 'scope))
        (model (cdr obj)))
    (loop model
       (cond
         ((list? (car model))
          (unless scope
            (for-each x
                      (model-find-elems (car model) id)
                      (push! result x)))
            (set! model (cdr model)))
         ((symbol? (car model))
            (when (and (eq (car model) :id)
                       (eq (cadr model) id))
              (push! result obj))
            (set! model (cddr model)))
         (else
          (set! model (cdr model)))))
    result))

(println (model-find-elems world-model 'tree))


(defun pset! (list key value)
  (loop list
       (when (eq (car list) key)
         (set! list (cdr list))
         (set-car! list value)
         (set! list nil))
       (set! list (cdr list))
       ))
(defun pget (list key)
  (let ((result nil))
    (loop list
         (when (eq (car list) key)
           (set! list (cdr list))
           (set! result (car list))
           (set! list nil))
         (set! list (cdr list))
         )
    result
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

(define ac-state :heat)
(define radio-state :off)
(define fox-happiness 100.0)
(define traveled 0.0)
(define money 0)
(define end-state nil)
(define been-close-set (make-hashtable))
(define biome :normal)
(define biome-offset 0.0)
(define dead-time 10.0)
(define show-help t)
(defun ambient-temperature ()
  (cond ((eq biome :normal) 50.0)
        ((eq biome :cold) 0.0)
        ((eq biome :hot) 100.0)
        (else 50.0)))

(defun reset ()
  (set show-help t)
  (set! biome :normal)
  (set! been-close-set  (make-hashtable))
  (set! car-road-location 1)
  (set! car-y-offset 0.0)
  (set! car-offset 1.3)
  (set! vx 0.0)
  (set! fox-happiness 100.0)
  (set! health-level 100.0)
  (set! temperature 50.0)
  (set! gas-level 100.0)
  (set! end-state nil)
  (set! traveled 0.0)
  (set! biome-offset)
  (set! dead-time 10.0)
  )
(reset)



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
      (- 0.0 x)
      x))


(defun render-scene()
  (measure2 'render-world
  (render-model
   `(root
     (transform :scale (2.0 2.0)
      (transform :translate (-0.5 -0.5)
       
       (color :rgb ,bg-color
        (ref square-model))
       ;; setting up the view
       (view :perspective (1.0 1.0 0.01 1000.0)
        (transform :translate (0 -4 -4)
         (transform :rotate (0.4 0.0 0)
          (transform :translate (-0.5 -0.5 -10)
           :scale (0.5 0.5 0.5)
           
           
           (ref world-model) )))))))))
  (measure2 'render-ui
  (render-model
   `(ui
     (transform :scale (2.0 2.0)
      (transform :translate (-0.5 -0.5)
       
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
        (transform :scale (0.03 -0.03)
         (transform :translate (25.0 -10.0)
          (color :rgb (0 0 0)
           (text ,(value->string money)))
          )
         ))

       
       (transform :scale (0.1 0.1) :translate (0.1 0.1)
        (transform :scale (10.0 2.0) :translate (-1.0 -1)
         (color :rgb (0.3 0.0 0.0)
          (ref square-model)))
        (transform :rotate (0 0 ,(if (eq ac-state :heat) -1.0
                                     (if (eq ac-state :cool)
                                         1.0
                                         0.0)))
         (ref dial-model)

         )
        (transform :translate (0.6 0.2) :scale (0.3 0.3)
         (color :rgb (,(if (eq? ac-state :heat) 1.0 0.3) 0.2 0.2)
          (ref square-model)))
        (transform :translate (-0.85 0.2) :scale (0.3 0.3)
         (color :rgb (0.2 0.2 ,(if (eq? ac-state :cool) 1.0 0.3))
          (ref square-model)))
        (transform :scale (0.015 -0.015) :translate (-0.3 1)
         (text "AC [E]"))
        )
       ;; radio
       (transform :scale (0.1 0.1) :translate (0.3 0.1)
        (transform :rotate (0 0 ,(if (eq? radio-state :off) 1.0 -1.0))
         (ref dial-model))
        (transform :translate (0.6 0.2) :scale (0.3 0.3)
         (color :rgb (0.2 ,(if (eq? radio-state :on) 1.0 0.3) 0.2)
          (ref square-model)))
        (transform :scale (0.015 -0.015) :translate (-0.3 1)
         (text "Radio [R]"))
        )
       )
      ))))
  (when show-help
    (render-model
     '(intro
       (transform :scale (2.0 2.0)
        
        (blend
         (transform :translate (-0.5 -0.5 0.0)
          (color :rgba (0 0 0 0.7)
           (ref square-model))))
        (transform :scale (0.1 0.1) :translate (-0.2 0.1)
         (transform :scale (0.013 -0.015) :translate (-0.3 1)
          (text "Welcome to Fox Driver.
You have to avoid dying.
You die by being too cold or too hot.
When you loose health, listen to the radio...
Also you have to avoid running out of gas...
So look at your bars.
You control the car with WASD.
Start by pressing W.
Restart by pressing [Enter]

"))
         )
        
        )

       )))

  )
(defun poll-events2 ()
  (let ((evts (foxgl:get-events)))
    (let ((evts2 (map evts (lambda (evt)
                             (let* ((tsl (cddr evt))
                                    (ts (car tsl))
                                    (tsl (cdr tsl))
                                    (e (car tsl)))
                               (cons e (cdr tsl)))))))
      evts2
      )))
(define mouse-x 0.0)
(define mouse-y 0.0)
(define mouse-0-down nil)
(define mouse-0-click nil)
(defun entities-update ()
  (set! mouse-0-click nil)
  (measure2 'poll-events
   (let ((evts (poll-events2))
         (s (foxgl:window-size win))
         )
    (when evts
      
      (println evts)
      )
    (map! (lambda (evt)
            (case (car evt)
              (mouse-move (let ((x (/ (rational (cadr evt)) (rational (car s))))
                                (y (/ (rational (cddr evt)) (rational (cadr s)))))
                            (set! mouse-x x)
                            (set! mouse-y y)
                            ))
              (mouse-button-down
               (when (eq (cadr evt) 0)
                 (unless mouse-0-down
                   (set! mouse-0-click t))
                 (set! mouse-0-down t))
               )
              (mouse-button-up
               (when (eq (cadr evt) 0)
                 (set! mouse-0-down nil))
              )))
          evts)
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
    (when (first (lambda (x)
                   (or (equals? '(key-down key 257) x)
                       (equals? '(key-down scankey 36) x))) evts)
      
      (reset))
    ))
  
  (measure2 'game-rest
  (let ((max-speed 0.7)
        (p1 nil)
        (p2 nil)
        (car-object (measure2 'car (model-find-elem world-model 'car)))
        (coin-object (measure2  'coin (model-find-elem world-model 'coin)))
        (gas-object (measure2 'gas (model-find-elem world-model 'gas)))
        (ice-object (measure 'ice-cube (model-find-elem world-model 'ice-cube)))
        (out-of-gas nil)
        (drive-left (< mouse-x 0.33))
        (drive-right (> mouse-x 0.66))
        (ac-area (and mouse-0-click (> mouse-y 0.8) (< mouse-x 0.2)))
        (radio-area (and mouse-0-click (> mouse-y 0.8) (> 0.4 mouse-x 0.2)))
        (road-part nil))

    (when ac-area
      (set! ac-state
            (cond ((eq ac-state :cool) :heat)
                  ((eq ac-state :heat) :off)
                  ((eq ac-state :off) :cool)
                  (else :cool))))
    (when radio-area
      (set! radio-state
            (cond ((eq radio-state :off) :on)
                  (else :off))))
    
    ;(println (list ac-area mouse-y mouse-x))

    (let ((out-of-bounds (> (abs car-y-offset) 6.0)))
      (when out-of-bounds
        (set! max-speed 0.2)
        ))
    (measure2 'game-events
              
    (when (or (foxgl:mouse-down? win 0) (foxgl:key-down? win foxgl:key-w))
      (when show-help
        (set! show-help nil))
      (set! vx (+ vx 0.1))
      )
    (when (foxgl:key-down? win foxgl:key-s)
      (set! vx (- vx 0.1))
      )
    
    (let ((left (or (and (< mouse-y 0.8) mouse-0-down (< mouse-x 0.33))
                    (foxgl:key-down? win foxgl:key-a)))
          (right (or (and (< mouse-y 0.8) mouse-0-down (> mouse-x 0.66))
                    (foxgl:key-down? win foxgl:key-d))))

      (if left
          (set! car-turn -0.3)
          (if right
              (set! car-turn  0.3)
              (if (< car-turn 0.0)
                  (set! car-turn -0.025)
                  (if (> car-turn 0.0)
                      (set! car-turn 0.025)))))))
    
    (incf car-rotation (* 1.5 car-turn))
    (when (> (abs vx) 0.1)
      (incf car-y-offset (- 0.0 (* max-speed car-turn))))
    (incf time 0.1)

    (when (> vx max-speed)
      (set! vx max-speed))
                                        ;(set! vx (* vx 0.9))

    (when (or end-state (< gas-level 0.00))
      (set! vx 0.0)
      (set! out-of-gas t)
      )
    (measure2 'list-offset
     (set! road-part (list-offset road-line (* car-road-location 2))))
    (unless (or show-help end-state out-of-gas)
      (when (eq ac-state :heat)
        (incf temperature 0.1))
      (when (eq ac-state :cool)
        (incf temperature -0.1))
      (when (/= ac-state :off)
        (incf gas-level -0.005))
      (when (eq radio-state :on)
        (incf gas-level -0.005))
      
      (if (and (eq radio-state :on) (< fox-happiness 100.0))
          (incf fox-happiness 0.05)
          (incf fox-happiness -0.05))

      (incf car-offset vx)
      (incf traveled vx)
      )
                                        ;(when (> traveled 100.0)
                                        ;  (lisp:exit))
    (when (> (- traveled biome-offset) 1000.0)
      (set! biome-offset traveled)
      (set! biome (cond ((eq biome :normal) :cold)
                        ((eq biome :cold) :hot)
                        (else :normal)))
      (when (eq biome :cold)
        (set! bg-color cold-color))
      (when (eq biome :hot)
        (set! bg-color hot-color))
      (when (eq biome :normal)
        (set! bg-color normal-color))    
      )
    
    (incf temperature (* 0.001 (- (ambient-temperature) temperature)))

    (measure2 'check-stats
    (if (< vx 0.0)
        (set! vx 0.0))
    (when (> vx 0.0)
      (incf gas-level -0.01))
    (when (< temperature 20.0)
      (incf health-level -0.1))
    (when (> temperature 80.0)
      (incf health-level -0.1))
    (when (< fox-happiness 20.0)
      (incf health-level -0.1))
    (when (> fox-happiness 80.0)
      (incf health-level 0.05))
    (set! health-level (min health-level 100.0))
    (when (< gas-level 0.001)
      (set! end-state :out-of-gas))
    (when (< health-level 0.001)
      (set! end-state :dead)))

    (set! p1 (vec2-scale (list->vec2 road-part) 20.0))
    
    (measure2 'check-collisions
    (for-each obj (list coin-object gas-object ice-object)
              (let* ((close (detect-collision car-object obj 20.0))
                     (collided (detect-collision car-object obj 1.5))
                     (gone (and (not close) (hashtable-ref been-close-set obj))))
                (when close 
                  (hashtable-set! been-close-set obj t)
                  )
              
              (when (or collided gone)
                (let* ((car-pos (list->vec3 (pget car-object :translate)))
                       (place (follow-lines road-part (/ (+ 50.0 (math:random 150.0)) 20.0)))
                       (next (vec2-scale (list->vec2 place) 20.0))
                       (next2 (vec2-scale (list->vec2 (cddr place)) 20.0))
                       (tangent (vec2-90 (vec2-normalize (vec2- next next2))))
                       (dir (if (eq (math:random 2) 1) -1 1))
                       (next3 (vec2+ next (vec2-scale tangent (* dir (+ -4 (math:random 8.0))))))
                       )
                  (pset! (cdr obj) :translate `(,(vec2-x next3)
                                                0.0
                                                ,(vec2-y next3)))
                  )
                (when collided
                  (println 'collision)
                  (when (eq (pget (cdr obj) :id ) 'gas)
                    (incf gas-level 3.0)
                    (set! gas-level (min 100.0 gas-level))
                    )
                  (when (eq (pget (cdr obj) :id ) 'coin)
                    (incf money 1)
                    )
                  (when (eq (pget (cdr obj) :id) 'ice-cube)
                    (set! temperature (- temperature 10))) 
                  )
                (when gone
                  (println 'gone))
                (hashtable-remove been-close-set obj)
                
                )))

    (for-each obj (list coin-object gas-object)
              (when obj
                (pset! (cdr obj) :rotate `(0.0 ,time 0.0)))))
    (unless (cdddr road-part)
      (set! car-offset 0.0)
      (set! car-road-location start-road-location))
    (measure2 'car-update
     (when (cdddr road-part)
      
      (set! p2 (vec2-scale (list->vec2 (list-offset road-part 2)) 20.0))
      (let* ((dv (vec2- p2 p1))
             (dvn (vec2-normalize dv))
             (dvt (vec2-90 dvn))
             (dvns (vec2-scale dvn car-offset))
             (cpos (vec2+ p1 dvns))
             
             (world-base (model-find-elem world-model 'world-base))
             )
        (when (> (vec2:len (vec2- cpos p1)) (vec2:len dv))
          (set! car-offset 0.0)
          (incf car-road-location 1)
          )
        (set! cpos (vec2+ cpos (vec2-scale dvt car-y-offset)))
        
        (let* ((car-dir (vec2-rotate (vec2 -1 0) car-rotation))
               (ang (vec2-dot car-dir dvn)))
          (set! car-rotation (+ car-rotation  (* (float32 0.5) ang)))
          )
        (pset! car-object :rotate `( 0.0 ,car-rotation 0.0))
        (pset! car-object :translate `(,(vec2-x cpos) 0.0 ,(vec2-y cpos)))
        (pset! world-base :translate `(,(- 0.0 (vec2-x cpos)) 0.0 ,(- 0.0 (vec2-y cpos))))
        )))
    (measure2 'update-trees
    (let* ((tree-scope (model-find-elem world-model 'trees))
           (trees (model-find-elems (cdr tree-scope) 'tree)))
      (let* ((car-pos (list->vec3 (pget car-object :translate)))
             (place (measure2 'follow (follow-lines road-part (/ (+ 50.0 (math:random 100.0)) 20.0))))
             (next (vec2-scale (list->vec2 place) 20.0))
             (next2 (vec2-scale (list->vec2 (cddr place)) 20.0))
             (tangent (vec2-90 (vec2-normalize (vec2- next next2))))
             (dir (if (eq (math:random 2) 1) -1.0 1.0))
             (next3 (vec2+ next (vec2-scale tangent (* dir (+ 5.0 (math:random 9.0))))))
             (moved nil)
             )
        (when (cddr place)
                                        ;(println (list next next2 tangent (math:random 10.0)))
          (map! (lambda (x)
                  
                  (let ((p-tree (list->vec3 (pget x :translate))))
                    
                    (let ((d (vec3-len (vec3- p-tree car-pos))))
                      (when (and (not moved) (> d 150.0))
                        (set! moved t)
                        (pset! (cdr x) :translate
                               `(,(vec2-x next3)
                                 0
                                 ,(vec2-y next3)))
                        
                        )
                      )))
                trees)
                                        ;(println p-car)

          )))))))
                                        ;
(defun game-update ()
                                        ;(audio:update)

  (measure2 'clear (foxgl:clear))
  (measure2 'entities-update
            (entities-update))
  (let ((s (foxgl:window-size win)))
    (foxgl:viewport (integer (car s)) (integer (cadr s))))
  (measure2 'render-scene
            (do-times 1 render-scene))
  (measure2 'swap
   ;(thread:sleep 0.005)
   (foxgl:swap win)
   (println (lisp:count-allocated))
                                        ;(let ((a (foxgl:timestamp)))
   (lisp:collect-garbage)
   ;  (println (- (foxgl:timestamp) a)))
   (foxgl:poll-events))
  )

(define ld50:initialized nil)
(define win nil)

(defun ld50:initialize()
  (set! win (foxgl:create-window (integer 800) (integer 800)))
  (foxgl:set-title win "Fox Driver")
  (foxgl:make-current win)
  (foxgl:load-font "DejaVuSans.ttf" (integer 22))
  )

(defun lisp:*web-update*()
  (unless ld50:initialized
    (set! ld50:initialized t)
    (ld50:initialize))
  (foxgl:make-current win)
  (with-exception-handler
      (progn                            ;(game-update)
        (game-update)
        )
    
    (lambda (x)
      (println x)
      (thread:sleep 0.5)
      ))

  )

(unless lisp:*web-environment*
  (define win (foxgl:create-window (integer 800) (integer 800)))
  (foxgl:set-title win "Fox Driver")
  (foxgl:make-current win)
  (foxgl:load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))

  (loop t
       (when swank-loaded
         (thread:lock-mutex *swank-mutex*))
       (with-exception-handler
           (measure2 'game-update 
            (game-update))
         (lambda (x)
           (println x)
           (thread:sleep 0.5)
           ))
       (when swank-loaded
         (thread:unlock-mutex *swank-mutex*))
       ))

