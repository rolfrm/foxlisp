
(load "lisp1.lisp")
(load "foxgl.lisp")

(define win (create-window (integer 800) (integer 600)))
(set-title win "Hello 2")
(make-current win)

(load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))
(define go t)

(register-finalizer (cons 1 2) (lambda (x) (println (list 'finalized x))))
(register-finalizer (cons 3 4) (lambda (x) (println (list 'finalized x))))

(let ((ht2 (make-hashtable nil nil)))
  (hashtable-set ht2 (cons 1 2) (cons 2 3)))

(defun test-f1 ()
  (let ((do-finalize (lambda (x) (println (list 'finalized x)))))
    (let ((ht2 (make-hashtable t nil))
          (item1 (cons 'aitem 2))
          (item2 (cons 'bitem 3)))
      (register-finalizer item1 do-finalize)
      (register-finalizer item2 do-finalize)
      (println 'finalize?)
      (hashtable-set ht2 item1 item2)
      ht2
      )
    ))

(define square-model '(polygon :2d-triangle-strip (0 0 1 0 0 1 1 1)))
(define trinagle '(polygon :2d-triangle-strip (0 0 0.5 1 1 0)))
(define bg-color '( 0.15 0.3 0.1))
(define tree
    `(color :rgb (0.6 0.4 0.2)
      (transform 
       (transform
        :translate (0 -0.5)
        :scale (0.2 1)
        ,square-model
        )
       (color :rgb (0.2 0.5 0.2)
        (transform :scale (0.75 0.75)
         :translate (-0.3 0.3)
         
         ,square-model
         (color :rgb (0.1 0.4 0.1)
          (transform :translate (0.5 -0.1) :scale (0.75 0.75) ,square-model))
         )
        
        ))))

(define rock
    `(color :rgb (0.4 0.4 0.4)
      (transform :translate (0.3 0.1)
       ,square-model)
      ,square-model))

(defun fmt (&rest args) "")


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
(load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))

(define txt nil)
(defun render-scene ()
  
  
  (let ((evts (poll-events2)))
    (map! (lambda (x)
            (when (eq (car x) 'key-down)
              (print x)))

          evts 
          )
    (when evts
                                        ;(println evts)
      ))
  
  (render-model
   `(root
     (color :rgb ,bg-color
      (transform :translate (-1 -1) :scale (2 2) ,square-model))
     (color :rgb (0.7 0.8 1.0)
      (transform :translate (-1 0.05) :scale (2 2) ,square-model))
     (transform :translate ( 0.0 0.0 0.0) 
      (color :rgb (1.0 1.0 0.0)
      
       (transform :translate (0.53 0.53) :scale (0.15 0.15) ,square-model)
       ))

     (transform :translate (,(* time -0.1) 0.0 0.0) 
     (color :rgb (1.0 1.0 1.0)
      
      (transform :translate (0.53 0.53) :scale (0.15 0.15) ,square-model)
      (transform :translate (0.3 0.5) :scale (0.3 0.3) ,square-model)
      (transform :translate (0.2 0.53) :scale (0.15 0.15) ,square-model)
      ))

     (view
      ;:orthographic (1.0 1.0 5.0) 
      :perspective (1.0 1.0 0.01 100.0)
      
     (transform :translate (,(- 0.0 time) 0 -5.0)
      
      (transform :translate (0.0 0.2 -3.0) ,tree)
      (transform :translate (0 -0.3 -2.5) :scale (0.2 0.2) ,rock)
      (transform :translate (-0.5 0.12 -2.0) ,tree)
      (transform :translate (0.45 0 -0.5)  ,tree)
      (color :rgb (0.9 0.9 0.1)
       (transform :scale (0.05 0.05) :translate (-0.7 -0.54)

        (transform :translate (1.5 -2.31) ,square-model)
        (transform :translate (-1.11 -1.13) ,square-model)
        ,square-model))
      
      
      (transform :scale (0.2 0.2) :translate (,(- time 0.25) -0.8 -0.25)
       :rotate (0 0 ,(* 0.5 (sin (* time 5.0))))
       (transform
        :translate (-1.0 2.0)
       (color :rgb (0.9 0.5 0.2)
        
        (transform :translate (0.5 -0.5) :scale (1 -1.0)
                                        ;(color ;:rgb (0 0 0)
         (transform :scale (0.2 1.25)
          (transform :translate (0 0)
           ,square-model)
          (transform :translate (4 0)
           ,square-model))
                                        ;)
         ,square-model))
       
       (color :rgb (0.9 0.9 0.9)
        (transform :translate (0.5 -0.5) :scale (1 -1.0) ,trinagle))
       (color :rgb (0.9 0.5 0.2)
        (polygon :2d-triangle-strip (0 0 1 -1 2 0))
        (polygon :2d-triangle-strip (0 0 0.2 0.4 0.4 0 ))
        (transform :translate (1.6 0) :scale (0.4 0.4) ,trinagle)
        )

       (color :rgb (0.8 0.3 0.2)
        (transform :translate (0.5 -0.5) :scale (1 -0.5)
         ,trinagle))
       (color :rgb (0 0 0)
        (transform :scale (0.3 -0.3) :translate (0.5 -0.1)
         ,trinagle))
       (color :rgb (0 0 0)
        (transform :scale (0.3 -0.3) :translate (1.2 -0.1)
         ,trinagle))
       ))
      )
      )
     (color :rgb (1 1 1 0.9)
      (transform :translate (0.6 -0.6) :scale (0.4 0.4)
       (transform :rotate (0.0 ,time ,time)
        (transform :translate (-0.5 -0.5)
         (flat :size (256 256)
          (color :rgb (1 1 1 0.5)
           (transform :scale (0.8 0.8) (polygon :2d-triangle-strip (-1 -1 -1 1 1 -1 1 1))))
          (color :rgb (1 1 0) (polygon :2d-triangle-strip (0 0 0.5 1 1 0)))
          (color :rgb (0 0 0)
           (transform :scale (1 -1) :translate (-40 40)
            (text "Text
text
text")
            ))
          (color :rgb (0 1 1) (polygon :2d-triangle-strip (0 0 0.5 -1 1 0)))
          
          ))))))
   )
  (when (> time 20.0)
    (set! time -10.0))
  )

(define time -2.0)
(define time-interval 0.05)
(loop t
     (render-scene)
     (swap win)
     (poll-events)
     (incf time time-interval)
     )
