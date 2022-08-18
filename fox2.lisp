
(load "lisp1.lisp")
(load "foxgl.lisp")

(defun sine-instrument (freq phase)
  (sin (* freq phase)))

         

(define song '(melody 0 2 4 8))
(define song-buffer (make-vector (* 44100 3) (float32)))
;; v1 1.17 sec
(progn
  (while t
         (measure
          (process-song song song-buffer 44100 0.0 1.0))
         (println 'done))
  ;;(set! sample1-loaded (audio:load-sample song-buffer))
 ;(println song-buffer)
 )


;

(define win (create-window (integer 800) (integer 600)))
(set-title win "Hello 2")
(make-current win)

(load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))
(define go t)

(register-finalizer (cons 1 2) (lambda (x) (println (list 'finalized x))))
(register-finalizer (cons 3 4) (lambda (x) (println (list 'finalized x))))

(let ((ht2 (make-hashtable)))
  (hashtable-set ht2 (cons 1 2) (cons 2 3)))

(defun test-f1 ()
  (let ((do-finalize (lambda (x) (println (list 'finalized x)))))
    (let ((ht2 (make-hashtable))
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

(defun fox (time jump)
  `(transform :scale (0.2 0.2) :translate (,(- time 0.25) ,(+ -0.8 (sin (* 3.14 (or jump 0.0)))) -0.25)
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
     )))

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

(define sample1 (make-vector (* 2 4096) (float32 0)))
(do-times (* 2 4096)
  (lambda (i) (vector-set! sample1  i (float32 (sin (* 0.02 (rational i)))))))

(println sample1)
(define sample1-loaded (audio:load-sample sample1))
(define txt nil)
(define jump nil)


(defun render-scene ()
  ;;(sleep 0.1)
  (audio:update)
  (when (and jump (< jump 1.0))
    (println jump)
    (incf jump 0.05))
  (when (and jump (> jump 1.0))
    (set! jump nil))
  
  (let ((evts (poll-events2)))
    (map! (lambda (x)
            (when (eq (car x) 'key-down)
              (match key (plookup (cdr x) 'scankey)
                     (when (and (eq key 65) (null? jump))
                       (set! jump 0.0)
                       (audio:play-sample (println sample1-loaded))
                       (println 'space)))))

          evts 
          )
    (when evts
      
      ;(println evts)
      ))
  
  (render-model
   ;asd
   `(root
     (color :rgb ,bg-color
      (transform :translate (-1 -1) :scale (2 2) ,square-model))
     (color :rgb (0.7 0.8 1.0)
      (transform :translate (-1 0.05) :scale (2 2) ,square-model))

     ;; sun
     (transform :translate ( 0.0 0.0 0.0) 
      (color :rgb (1.0 1.0 0.0)
      
       (transform :translate (0.53 0.57) :scale (0.15 0.15) ,square-model)
       
       ))

     (transform :translate (,(* time -0.1) 0.0 0.0)
      ;; cloud
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
      
      
      ,(fox time jump)
      )
      )
     (color :rgb (1 1 1 0.9)
      ;; <trasform translate="(0.6, 0.6)" scale="(0.5, 0.5)
      ;;
      
      (transform :translate (0.6 -0.6) :scale (0.4 0.4)
       (transform :rotate (0.0 ,time ,time)
        (transform :translate (-0.5 -0.5)
         (flat :size (256 256)
          (color :rgb (1 1 1 0.5)
           (transform :scale (0.8 0.8)
            (polygon :2d-triangle-strip (-1 -1 -1 1 1 -1 1 1))))
          (color :rgb (1 1 0) (polygon :2d-triangle-strip (0 0 0.5 1 1 0)))
          (color :rgb (0 1 1) (polygon :2d-triangle-strip (0 0 0.5 -1 1 0)))
          
          (color :rgb (0 0 0)
           (transform :scale (1 -1) :translate (-40 40)
            (text "Hej
Sebasianowich")
            ))
          
          ))))))
   )
  (when (> time 20.0)
    (set! time -10.0))
  )

(define time -2.0)
(define time-interval 0.05)
(loop t
      (thread:lock-mutex *swank-mutex*)
      (with-exception-handler
          (render-scene)
        (lambda (x)
          (println x)
          (sleep 0.5)
          ))
      (swap win)
      (poll-events)
      (incf time time-interval)
      (thread:unlock-mutex *swank-mutex*)
     )

