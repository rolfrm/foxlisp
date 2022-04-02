(load "lisp1.lisp")
(load "foxgl.lisp")
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
       ))
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
  
  ;;(incf time 0.3)
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
        :rotate (0 0 0)
        ,heart-model)
       (transform :scale (0.05 0.05) :translate (0.05 0.85)
                  :rotate (0 0 0)
        ,gas-model)
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
  (render-scene)
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
     )

