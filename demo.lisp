(define window-title "funky")
(load "models.lisp")


(defvar +dt+ 0.0)
(defvar +dt2+ 0.0)
(defvar zoom 0.1)
(defun game-update(events)
  (for-each x events
            (when (eq (car x) 'mouse-scroll)
              (let* ((amount (caddr x))
                     (amount2 (math:pow 1.2 amount)))
                (set! zoom (* zoom amount2))
                (println zoom)))
            (when (eq (car x) 'mouse-move)
                      (set! +dt+ (* 0.01 (rational(cddr  x))))
                      (set! +dt2+ (* 0.01 (rational(cadr  x))))

                      ))
  )

(defvar center-cube
    '(translate (-0.5 -0.5 -0.5)
      (bind cube-model)))


(defvar model-c '(rgb (0.8 0.8 0.8)
                 (scale 0.9
                  (bind center-cube))

                 (translate (0.0 0.09 0.0)
                  (scale (1.0 0.9 1.0)
                   (rgb (0.5 0.5 0.5)
                    (scale (0.1 1.1 1.1)
                   (bind center-cube)
                   ) 
                  (rotate (0.0 (bind (* 0.5 pi)) 0.0)
                   (scale (0.1 1.1 1.1)
                    (bind center-cube)
                    )))))))

(defvar model-b '(rgb (0.8 0.8 0.8)
                  (translate (0 1 0)
                   (translate (0.0 0.0 0.0)
                    (rgb (0.5 0.5 0.5)
                     (scale (0.25 0.5 0.25)
                      (bind cube-model))
                     ))
                   (translate (0.25 0.0 0.25)
                    (rgb (0.4 0.4 0.4)
                     (scale (0.5 0.4 0.25)
                      (bind cube-model))
                     ))
                   (translate (0.0 0.0 0.25)
                    (rgb (0.35 0.35 0.35)
                     (scale (0.25 0.3 0.25)
                      (bind cube-model))
                     ))
                   (translate (0.0 0.0 0.5)
                    (rgb (0.25 0.25 0.25)
                     (scale (0.5 0.25 0.5)
                      (bind cube-model))))
                   (translate (0.5 0.0 0.5)
                    (rgb (0.33 0.33 0.25)
                     (scale (0.5 0.15 0.5)
                      (bind cube-model))))
                   )

                   
                   (bind cube-model))
)

(define start-time (foxgl:timestamp))
(define model
    '(view :perspective (1.0 1.0 0.01 100.0)
      (depth
       (translate (-0.0 -0.0 -2)
        (scale (bind zoom)
         (rotate ((bind +dt+) (bind +dt2+) 0.0)
          (for i (-6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8)
           (for j (-6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8)
            (translate ((bind i) 0 (bind j))
             (translate (0.5 0 0.5)
             (rotate (0 (bind (+ pi (* i 5) j)) 0)
              (translate (-0.5 0 -0.5)
              (bind model-b))))
            )))))))

        ))
