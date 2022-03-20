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
(define time 0.0)
(loop t

      ;(render-model '(color :rgb (0.1 0.1 0.1)))
      
      (render-model
       `(root
         (color :rgb (0.1 0.1 0.1) 
                (transform :translate (-1 -1) :scale (2 2) ,square-model))
         
         (color :rgb (0.9 0.9 0.5)
               (transform :scale (0.2 0.2)
                          ,square-model
                          )
               (color :rgb (0.5 0.5 1.0)
                      (transform :scale (0.1 0.1)
                                 :translate (0.1 0.2)
                                 ,square-model
                                 ))
               (transform :translate (,(* 0.1 (sin time)) -0.5) :scale (0.2 0.2)
                          (color :rgb (0.9 0.5 0.2)
                                 (polygon :2d-triangle-strip (0 0 1 -1 2 0))
                                 (polygon :2d-triangle-strip (0 0 0.2 0.4 0.4 0 ))
                                 (transform :translate (1.6 0)
                                            (polygon :2d-triangle-strip (0 0 0.2 0.4 0.4 0 )))
                                 ))
               )))

     (swap win)
     (poll-events)
     (incf time 0.1)
                                        ;(set! go nil)
     )
