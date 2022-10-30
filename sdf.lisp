

(defun foxgl:build-sub-sdf (model)
  (plist-rest model foxgl:build-sdf0))

(defvar sdf-out nil)

(defun foxgl:build-sdf(model cscope)
  (set! sdf-out (list 'sdf))
  (let ((r (foxgl:build-sdf0 model)))
    (set! foxgl:current-scope cscope)
    (swap sdf-out nil)
  ))

(defun foxgl:build-sdf0 (model)
  (case (car model)
    (aabb
     (push-back! sdf-out model)
     )
    (sphere
     (push-back! sdf-out (list 'sphere (unbind (cadr model))))
     )
    (capsule
     (push-back! sdf-out model)
     )
    (sphere-bounds
     (let* ((prev-sdf sdf-out))
       (set! sdf-out (list 'sphere-bounds))
       (foxgl:build-sub-sdf (cdr model))
       (set! model nil)
       (let ((m sdf-out))
         (set! sdf-out prev-sdf)
         (push-back! sdf-out m))))
    (soft
     (let* ((soft (unbind (cadr model)))
            (prev-sdf sdf-out)
            )
       (set! sdf-out (list 'soft soft))
       (foxgl:build-sub-sdf (cddr model))
       (set! model nil)
       (let ((m sdf-out))
         (set! sdf-out prev-sdf)
         (push-back! sdf-out m))))
    (subtract
     (let* ((k (unbind (cadr model)))
            (prev-sdf sdf-out)
            )
       (set! sdf-out (list 'subtract k))
       (foxgl:build-sub-sdf (cddr model))
       (set! model nil)
       (let ((m sdf-out))
         (set! sdf-out prev-sdf)
         (push-back! sdf-out m))))
    (color )
    (rgb
     (let* ((color (unbind (cadr model)))
            (r (unbind (car color)))
            (g         (unbind (cadr color)))
            (b         (unbind (caddr color)))
            (a         (unbind (cadddr color)))
            (prev-sdf sdf-out)
            )
       (set! sdf-out (list 'rgb (list r g b)))
       (foxgl:build-sub-sdf (cddr model))
       (set! model nil)
       (let ((m sdf-out))
         (set! sdf-out prev-sdf)
         (push-back! sdf-out m))))
    (color )
    (measure-model )
    (ref (foxgl:build-sdf0 (symbol-value  (unbind (cadr model)))))
    (bind (foxgl:build-sdf0 (eval (cadr model) foxgl:current-scope)))
    (for
     (let* ((var (cadr model))
            (evalues (or (unbind (caddr model)) nil))
            (rest (cdddr model))
            (prev-scope foxgl:current-scope))
       (lisp:with-scope-variable prev-scope (lisp:get-current-scope!!) '(evalues rest var) var
          '(
            (set! foxgl:current-scope (lisp:get-current-scope!!))
            (loop evalues
                 (lisp:scope-set! foxgl:current-scope var (unbind (car evalues)))
                 (foxgl:build-sub-sdf rest)
                 (set! evalues (cdr evalues)))
            ))
       (set! foxgl:current-scope prev-scope)
       (set! model nil)
       ))
    
    (let
        (let ((vars (cadr model))
              (body (cddr model))
              (prev-scope foxgl:current-scope))
          (lisp:with-scope-binding prev-scope (lisp:get-current-scope) '(body) vars
                                   '((set! foxgl:current-scope (lisp:get-current-scope))
                                     
                                     (foxgl:build-sub-sdf body)

                   ))
      
      
      (set! model nil)
      (set! foxgl:current-scope prev-scope)))
          
    (view )
    (rotate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind (cadr model))))
       (if (cons? rot)
           (math:rotate!  new-transform
                          (unbind (car rot))
                          (or (unbind (cadr rot)) 0.0)
                          (or (unbind (caddr rot)) 0.0))
           (math:rotate!  new-transform
                          0.0
                          0.0
                          (unbind rot))
           )
       (let ((prev-sdf (swap sdf-out (list 'transform (clone-mat4 new-transform) )))) 

         (foxgl:build-sub-sdf (cddr model))
         (let ((new (swap sdf-out prev-sdf)))
           
           (push-back! sdf-out new)
           (set! model nil)
           (release-matrix new-transform)
           ))
       ))
    (translate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind (cadr model))))
       (math:translate! new-transform
                        (unbind (car rot))
                        (or (unbind (cadr rot)) 0.0)
                        (or (unbind (caddr rot)) 0.0))
       (let ((prev-sdf (swap sdf-out (list 'transform (clone-mat4 new-transform) )))) 

         (foxgl:build-sub-sdf (cddr model))
         (let ((new (swap sdf-out prev-sdf)))
           
           (push-back! sdf-out new)
           (set! model nil)
           (release-matrix new-transform)
           ))))
    (scale
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind (cadr model)))
           )
       (math:*! new-transform new-transform foxgl:current-transform)
       (if (list? rot)
           (math:scale! new-transform
                        (unbind (car rot))
                        (or (unbind (cadr rot)) 1.0)
                        (or (unbind (caddr rot)) 1.0))
           (math:scale! new-transform
                        rot rot rot))
       (let ((prev-sdf (swap sdf-out (list 'transform (clone-mat4 new-transform) )))) 
         
         (foxgl:build-sub-sdf (cddr model))
         (let ((new (swap sdf-out prev-sdf)))
           (set! model nil)
           (push-back! sdf-out new)
           (release-matrix new-transform)
           ))))
    
    (skew ;; unsupported transform
     )
    (transform
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix)))
       (when foxgl:current-transform
         (math:*! new-transform new-transform foxgl:current-transform))
       
       (match tlate (unbind (plookup (cdr model) ':translate))
              (math:translate! new-transform
                               (unbind (car tlate))
                               (unbind (cadr tlate))
                               (or (unbind (caddr tlate)) 0.0)
                               ))
       
       
       (match scale (unbind (plookup (cdr model) ':scale))
              (math:scale! new-transform 
                           (car scale) (cadr scale) (or (caddr scale) 1.0)))
       
       (match rotation (unbind (plookup (cdr model) ':rotate))
              
              (math:rotate!  new-transform
                             (unbind (car rotation))
                             (or (unbind (cadr rotation)) 0.0)
                             (or (unbind (caddr rotation)) 0.0)))
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-sdf (cdr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (render-callback )
    (unit-square )
    (print-model)
    (blend )
    (depth )
    (text )
    (flat)
    (line )
    (point )
    (sdf)
    (bake)
    (polygon)
    (hidden
     (set! model nil)))
  (when (cdr model)
    (foxgl:build-sub-sdf (cdr model)))
  )
