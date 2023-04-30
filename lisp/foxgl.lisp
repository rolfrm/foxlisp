													 ;(println (foxgl:timestamp))
(defun spherex(c r v)
  (- (vec3-len (vec3- v c)) r) 
  )
(defmacro measure(&rest body)
  `(let ((time-start (foxgl:timestamp))
         (result (progn ,@body))
         (time-end (foxgl:timestamp)))
                                        ;(println (list 'operation-took (/ (rational (- time-end time-start)) 1000000.0)))
     result))

(define measure2-inlet 0)
(defmacro measure2 (name &rest body)
  `(let ((time-start (foxgl:timestamp))
         (l (incf measure2-inlet 1))
			
         (result (progn ,@body))
         (time-end (foxgl:timestamp)))
     
     (println (list l ,name (/ (rational (- time-end time-start)) 1000000.0)))
                                        ;  )
     (incf measure2-inlet -1)
     result))

(defmacro measure2 (name &rest body)
  `(progn ,@body))


(defmacro with-tracing(&rest body)
  `(let ((result nil))
     (lisp:trace t)
     (set! result
			  (progn ,@body))
     (lisp:trace nil)
     result))

(defun plist-rest (lst func)
  (while lst
    (let ((fst (car lst)))
      (if (symbol? fst)
          (set! lst (cddr lst))
          (do
           (set! lst (cdr lst))
           (func fst))))))

(defun mat4-identity ()
  (let ((m (make-vector 16 (float32 0.0))))
    (mat4:identity! m)
    m
    ))
(defun mat4-print (mat)
  (dotimes! i 4
            (dotimes! j 4
                      
							 (print (vector-ref mat (+ (* j 4) i)))
							 (print " "))
				(println ""))
  mat
  )

(defun mat4-translation (x y z)
  (let ((m (mat4-identity)))
    (vector-set! m 12 (float32 x))
    (vector-set! m 13 (float32 y))
    (vector-set! m 14 (float32 z))
    m))

(defun mat4-scale (x y z)
  (let ((m (mat4-identity)))
    (vector-set! m 0 (float32 x))
    (vector-set! m 5 (float32 y))
    (vector-set! m 10 (float32 z))
    m))
(defun mat3-identity ()
  (let ((m (make-vector 9 (float32 0.0))))
    (vector-set! m 0 (float32 1.0))
    (vector-set! m 4 (float32 1.0))
    (vector-set! m 8 (float32 1.0))
    m
    ))

(defun mat3-scale (x y)
  (let ((m (mat3-identity)))
    (vector-set! m 0 (float32 x))
    (vector-set! m 4 (float32 y))
    m))

(defun render-model (model)
  (foxgl:init)
													 ;(println 'render-model)
  (foxgl:render-model2 model)
  )

(defmacro foxgl:render-model(model)
  `(let ((cscope foxgl:current-scope))
     (set! foxgl:current-scope (lisp:get-current-scope))
     (render-model ,model)
     (set! foxgl:current-scope cscope)))

(define get-framebuffer nil)
(define foxgl:current-color '(1 1 1 1))
(define foxgl:current-transform (mat4-identity))
(define foxgl:framebuffer-cache (make-hashtable :keys 2))
(define foxgl:polygon-cache2 nil)
(define foxgl:polygon-cache (make-hashtable))
(define foxgl:square-buffers nil)
(define foxgl:baking nil)
(define foxgl:baking-stack nil)
(define foxgl:building-sdf nil)
(define foxgl:sdf-stack nil)
(define foxgl:sdf-clip (make-hashtable))
(define foxgl:sdf-build-lookup (make-hashtable))

(defun -compare-clip (now-clip prev-clip size)
  (when prev-clip
    (and (<  (abs (- (car now-clip) (car prev-clip))) (/ size 2.0))
			(< (abs (- (cadr now-clip) (cadr prev-clip))) (/ size 2))
			(< (abs (- (caddr now-clip) (caddr prev-clip))) (/ size 2)))))

(defun calc-clip (now-clip size)
  (let ((s2 size))
    (list (* (math:floor (/ (car now-clip) s2)) s2)
          (* (math:floor (/ (cadr now-clip) s2)) s2)
          (* (math:floor (/ (caddr now-clip) s2)) s2))))

(defvar sdf-chunk-lookup (make-hashtable :deep-equality))
(defvar sdf-chunk-lookup-model-id (make-hashtable))
(defvar sdf-chunk-model-id-c 0)
(defun render-sdf-clip (model size resolution clip)
  (let ((model-id (hashtable-ref sdf-chunk-lookup-model-id model))
        (sdf-model nil))
    (unless model-id
													 ;(println 'recalc-chunk-id)
      (incf sdf-chunk-model-id-c)
      (set! model-id sdf-chunk-model-id-c)
      (hashtable-set sdf-chunk-lookup-model-id model model-id))
    (dotimes! o 2
				  (let* ((s2 (integer (* (math:pow 2.0 (rational o)) size)))
							(p (calc-clip clip s2 ))
							)
					 (dotimes! i 6
								  (dotimes! j 6
												(dotimes! k 6
															 (when (and (< o 2) (< o 3) (or (eq 0 o) (not (and (or (= j 2) (= j 3))
																																(or (= i 2) (= i 3))
																																(or (= k 2) (= k 3))))))
																(let ((x (+ (* s2 (- i 2)) (* s2 1 (math:round (/ (car p) (* s2 1))))))
																		(y (+ (* s2 (- j 1)) (cadr p)))
																		(z (+ (* s2 (- k 2)) (* s2 1 (math:round (/ (caddr p) (* s2 1))))))
																		(key (list model-id o x y z)))
													 ;(println (list o x y z))
																  (let ((r (hashtable-ref sdf-chunk-lookup key)))
																	 (unless r
																		(println (list 'eeeh_ x y z s2))
																		(unless sdf-model
																		  (set! sdf-model (foxgl:build-sdf (cdr model) foxgl:current-scope)))
																		(let
																			 ((poly (foxgl:sdf-marching-cubes (* 0.5 s2) (* (math:pow 2.0 (rational o)) resolution) sdf-model (list x y z))
																				 ))
																		  (set! r (cons 'poly (list (foxgl:load-polygon (car poly) 3 nil nil :triangles-color)
																											 (foxgl:load-polygon (cdr poly) 3 nil nil :triangles-color)
																											 )))
																		  (println (list 'recalc sdf-chunk-lookup))
																		  (hashtable-set sdf-chunk-lookup key r)
																		  ))
																	 
																	 (foxgl:color foxgl:current-color)
																	 (foxgl:transform foxgl:current-transform)
																	 
																	 (foxgl:blit-mode :triangles-color)
																	 (foxgl:blit-polygon (cdr r))
																	 (foxgl:blit-mode nil)
																	 )
																  )
																)
															 )
												)
								  )))))


(defun foxgl:render-sub-models (model)
  (plist-rest model foxgl:render-model2))
(define foxgl:test-tex nil)

(defun foxgl:get-framebuffer (model key2)
  (hashtable-refn foxgl:framebuffer-cache model key2))
(defun foxgl:load-framebuffer (model size key2)
  (let ((bf (foxgl:create-framebuffer (car size) (cadr size))))
	 (hashtable-setn! foxgl:framebuffer-cache model key2 bf)
    bf))

(define foxgl:current-scope nil)

(defun unbind2(p)
  (unbind p foxgl:current-scope))

(defmacro !render-sub (modelx)
  `(progn
     (foxgl:render-sub-models ,modelx)
     (set! model nil)
     ))
(defvar matrix-cache nil)
(defun prepare-matrix(m)
  
  (mat4:identity! m)
  m)

(defun get-matrix()
  (if matrix-cache
      (prepare-matrix (pop! matrix-cache))
      (mat4-identity)))

(defun release-matrix(mat)
  (push! matrix-cache mat)
  )


(define foxgl:-points-array (make-vector 32 (float32 0.0)))
(define foxgl:-points-array-count 0)

(defun foxgl:render-model2 (model)
  (case (car model)
    (rgb
     (let* ((prev-color foxgl:current-color)
            (color (unbind2 (cadr model)))
            (color2 (foxgl:color->int
                     (unbind2 (car color))
                     (unbind2 (cadr color))
                     (unbind2 (caddr color))
                     (unbind2 (cadddr color))))
            )
       (set! foxgl:current-color color2)
       (!render-sub (cddr model))
       (set! foxgl:current-color prev-color)
       ))
    (color
     (let ((prev-color foxgl:current-color))
       
       (match rgb (plookup (cdr model) ':rgb)
              (set! foxgl:current-color rgb)
              )
       (match rgb (plookup (cdr model) ':rgba)
              (set! foxgl:current-color rgb)
              )
       (foxgl:render-sub-models (cdr model))
       (set! model nil)
       (set! foxgl:current-color prev-color)
       ))
    (measure-model
     (measure (foxgl:render-model2 (cadr model)))
     (set! model nil)
     )
    (ref
     
     (foxgl:render-model2 (symbol-value  (unbind2 (cadr model))) t))
    (bind
     (foxgl:render-model2 (eval (cadr model) foxgl:current-scope)))
    (for
     (let* ((var (cadr model))
            (evalues (or (unbind2 (caddr model)) nil))
            (rest (cdddr model))
            (prev-scope foxgl:current-scope))
       (lisp:with-scope-variable prev-scope (lisp:get-current-scope!!) '(evalues rest var) var
											'(
											  (set! foxgl:current-scope (lisp:get-current-scope!!))
											  (loop evalues
													 (lisp:scope-set! foxgl:current-scope var (unbind2 (car evalues)))
													 (foxgl:render-sub-models rest)
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
                                     (foxgl:render-sub-models body)

												 ))
			 
			 
			 (set! model nil)
			 (set! foxgl:current-scope prev-scope)))
    
    (view
     (let ((prev-transform foxgl:current-transform))
       (match p (unbind2 (plookup (cdr model) :perspective))
              (let ((fov (unbind2 (car p)))
                    (aspect (unbind2 (cadr p)))
                    (near (caddr p))
                    (far (cadddr p))
                    (prev-tform foxgl:current-transform))
                (set! foxgl:current-transform (mat4:perspective fov aspect near far))
                ))
       (match p (plookup (cdr model) :orthographic)
              (let ((w (unbind2 (car p)))
                    (h (unbind2 (cadr p)))
                    (z (unbind2 (caddr p)))
                    (prev-tform foxgl:current-transform))
                (set! foxgl:current-transform (mat4:orthographic w h z))
                ))
       (foxgl:render-sub-models (cdr model))
       (set! model nil)
       (set! foxgl:current-transform prev-transform)
       ))
    (rotate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (if (cons? rot)
           (math:rotate!  new-transform
                          (unbind2 (car rot))
                          (or (unbind2 (cadr rot)) 0.0)
                          (or (unbind2 (caddr rot)) 0.0))
           (math:rotate!  new-transform
                          0.0
                          0.0
                          (unbind2 rot))
           )
       (set! foxgl:current-transform new-transform)
       (foxgl:render-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (translate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (math:translate! new-transform
                        (unbind2 (car rot))
                        (or (unbind2 (cadr rot)) 0.0)
                        (or (unbind2 (caddr rot)) 0.0))
       
       (set! foxgl:current-transform new-transform)
       (foxgl:render-sub-models (cddr model))
       (set! foxgl:current-transform prev-tform)
       (set! model nil)
       (release-matrix new-transform)
       ))
    (scale
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (if (list? rot)
           (math:scale! new-transform
                        (unbind2 (car rot))
                        (or (unbind2 (cadr rot)) 1.0)
                        (or (unbind2 (caddr rot)) 1.0))
           (math:scale! new-transform
                        rot rot rot))
       
       
       (set! foxgl:current-transform new-transform)
       (foxgl:render-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (skew
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (skew (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (math:skew! new-transform skew)
													 ;(println skew)
       
       (set! foxgl:current-transform new-transform)
       (foxgl:render-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (transform
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix)))
       (when foxgl:current-transform
         (math:*! new-transform new-transform foxgl:current-transform))
       
       (match tlate (unbind2 (plookup (cdr model) ':translate))
              (math:translate! new-transform
                               (unbind2 (car tlate))
                               (unbind2 (cadr tlate))
                               (or (unbind2 (caddr tlate)) 0.0)
                               ))
       
       
       (match scale (unbind2 (plookup (cdr model) ':scale))
              (math:scale! new-transform 
                           (car scale) (cadr scale) (or (caddr scale) 1.0)))
       
       (match rotation (unbind2 (plookup (cdr model) ':rotate))
              
              (math:rotate!  new-transform
                             (unbind2 (car rotation))
                             (or (unbind2 (cadr rotation)) 0.0)
                             (or (unbind2 (caddr rotation)) 0.0)))
       (set! foxgl:current-transform new-transform)
       (foxgl:render-sub-models (cdr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (render-callback
     ((cadr model) model)
     )
    (unit-square
     (foxgl:color foxgl:current-color)
     (foxgl:transform (or foxgl:current-transform (mat4-identity)))
     (foxgl:square)
     )
    (print-model
     (println 'print-model)
     (println (cdr model))

     )
    (blend
     (foxgl:blend t)
     (foxgl:render-sub-models (cdr model))
     (set! model nil)
     (foxgl:blend nil))
    (depth
     (foxgl:depth 123)
     (foxgl:render-sub-models (cdr model))
     (set! model nil)
     (foxgl:depth nil)
     )
    (text
     (foxgl:color foxgl:current-color)
     (foxgl:transform (or foxgl:current-transform (mat4-identity)))
     (foxgl:blit-text (value->string (unbind2 (cadr model))) (or foxgl:current-transform (mat4-identity)))
     )
    (flat
     (let ((fb (foxgl:get-framebuffer model))
           (prev-transform foxgl:current-transform)
           )
       (when (null? fb)
         (let ((s (plookup (cdr model) :size)))
           (set! fb (foxgl:load-framebuffer model (or s '(100 100))))))
       (foxgl:bind-framebuffer fb)
	    
       (set! foxgl:transform (mat4-identity))
       (foxgl:unbind-framebuffer fb)
       (set! foxgl:current-transform prev-transform)
       (unless foxgl:square-buffers
         
         (set! foxgl:square-buffers
               (list
                (foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))
                (foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))))
         
         )
       (foxgl:bind-texture (foxgl:framebuffer-texture fb))
       (foxgl:color foxgl:current-color)
       (foxgl:transform foxgl:current-transform)
       (foxgl:blend t)
       (foxgl:blit-polygon foxgl:square-buffers)
       (foxgl:blend nil)
       (foxgl:bind-texture nil)
       (set! model nil)
       ))
    (line
     (set! foxgl:-points-array-count 0)
     (foxgl:render-sub-models (cdr model))
     
     (when (> (* foxgl:-points-array-count 2) (vector-length foxgl:-points-array)) 
       (set! foxgl:-points-array (vector-resize foxgl:-points-array (* (vector-length foxgl:-points-array) 2))))
     (let ((cnt (/ foxgl:-points-array-count 2))
           (array foxgl:-points-array)
           (line-width 0.3))
       (dotimes! _i cnt
                 (let ((i (- (- cnt _i) 1)))
                   (let ((i1 (* i 2))
                         (i2 (+ (* i 2) 1)))
													 ;(println (cons i1 i2))
							(let ((x1 (vector-ref array i1))
									(y1 (vector-ref array i2)))
							  (let ((x2 x1)
									  (y2 y1))
								 (when (> i 0)
									(set! x2 (vector-ref array (- i1 2)))
									(set! y2 (vector-ref array (- i2 2))))
								 (when (= i 0)
									(set! x2 (vector-ref array (+ i1 2)))
									(set! y2 (vector-ref array (+ i2 2))))
								 
								 (let ((dx (- x2 x1))
										 (dy (- y2 y1)))
									(let ((l (math:sqrtf (+ (* dx dx) (* dy dy)))))
									  
									  (set! dx (/ dx l))
									  (set! dy (/ dy l))
									  )
									(when (> i 0)
									  (set! dx (* -1 dx)))
													 ;(println (list dx dy x1 y1 x2 y2))
									(set! dx (float32 (* dx line-width 0.5)))
									(set! dy (float32 (* dy line-width 0.5)))
									
									(let ((i3 (* i1 2)))
													 ;(println i3)
									  (vector-set! array (+ i3 2) (- x1 dy))
									  (vector-set! array (+ i3 3) (- y1 dx))
									  (vector-set! array (+ i3 0) (+ x1 dy))
									  (vector-set! array (+ i3 1) (+ y1 dx)))
									
									))))))
													 ;(print array)
       (let ((poly (foxgl:load-polygon array 2 0 (* foxgl:-points-array-count 2))))
         (foxgl:color foxgl:current-color)
         (foxgl:transform foxgl:current-transform)
         (foxgl:blit-polygon poly)
         (foxgl:delete-polygon poly)
			))
     
     (set! model nil)
     )
    (point
     (let ((x (unbind2 (cadr model)))
           (y (unbind2 (caddr model))))
       (when (> (+ foxgl:-points-array-count 2) (vector-length foxgl:-points-array)) 
         (set! foxgl:-points-array (vector-resize foxgl:-points-array (* (vector-length foxgl:-points-array) 2)))

         )
       

       (vector-set! foxgl:-points-array foxgl:-points-array-count (float32 x))
       (vector-set! foxgl:-points-array (+ foxgl:-points-array-count 1) (float32 y))
       (incf foxgl:-points-array-count 2)
       )
     
     (set! model nil)
     )
    (sdf
     (let ((size (plookup (cdr model) :size))
           (resolution (plookup (cdr model) :resolution))
           (clip (unbind2 (plookup (cdr model) :clip)))
           (position (unbind2 (plookup (cdr model) :position)))
           (positions nil)
           )
       (when clip
         (let ((sdf-build (hashtable-ref foxgl:sdf-build-lookup (cdr model))))
           (unless sdf-build
             (set! sdf-build (foxgl:build-sdf (cdr model) foxgl:current-scope)))
           (hashtable-set foxgl:sdf-build-lookup (cdr model) sdf-build)
													 ;(render-sdf-clip model size resolution clip)
           (sdf:render sdf-build foxgl:current-transform clip)
           ))
       (unless clip
			(set! foxgl:sdf-stack nil)
			(let ((r (hashtable-ref foxgl:polygon-cache model)))
			  (unless r
				 (println (list 'position position))
				 (let* ((model2 (println (foxgl:build-sdf (cdr model) foxgl:current-scope)) )
						  
						  (poly
							(foxgl:sdf-marching-cubes size resolution model2 (println position))
							 ))
					(set! r (cons 'poly (list (foxgl:load-polygon (car poly) 3 nil nil)
													  (foxgl:load-polygon (cdr poly) 3 nil nil)
													  )))
					(hashtable-set foxgl:polygon-cache model r)
					
					))
			  
			  (foxgl:color foxgl:current-color)
			  (foxgl:transform foxgl:current-transform)
			  (foxgl:blit-mode :triangles-color)
			  (foxgl:blit-polygon (cdr r))
			  (foxgl:blit-mode nil)
			  
			  ))
       (set! model nil)
       ))
	 
    (bake
     (let ((r (hashtable-ref foxgl:polygon-cache (cdr model))))
       (unless r
         (let ((tform foxgl:current-transform))
           (set! foxgl:baking t)
                                        ;(set! model nil)
           (foxgl:render-sub-models (cdr model))
           (let ((baked (foxgl:bake foxgl:baking-stack tform)))
             (set! r (cons 'bake (list (foxgl:load-polygon (car baked) 3 nil nil)
                                       (foxgl:load-polygon (cdr baked) 3 nil nil)
                                       )))
             (hashtable-set foxgl:polygon-cache (cdr model) r)
				 
             )
           
           (set! foxgl:baking nil)
           (set! foxgl:baking-stack nil)
           ))
       (set! model nil)
       
       (foxgl:color '(1 1 1 1))
       (foxgl:transform foxgl:current-transform)
       (foxgl:blit-mode :triangle-strip-color)
       (foxgl:blit-polygon (cdr r))
       (foxgl:blit-mode nil)
       
       ))
    (polygon
     (let ((dims 2)
           (poly (plookup (cdr model) :2d-triangle-strip)))
       (unless poly
         (set! poly (plookup (cdr model) :3d-triangle-strip))
         (set! dims 3)
         )
       (when poly
         (if foxgl:baking
             (progn
               
               (push! foxgl:baking-stack
                      (list foxgl:current-color
                            (math:*
                             foxgl:current-transform
                             mat4:*identity*)
                            (list-to-array poly) dims))
               
               )
             
             
				 (let ((r (hashtable-ref foxgl:polygon-cache (cdr model))))
					(unless r
					  (println (list 'new-poly r))
					  (set! r (cons 'poly (foxgl:load-polygon (list-to-array poly) dims)))
					  (hashtable-set foxgl:polygon-cache (cdr model) r)
					  (push! foxgl:polygon-cache2 (cdr model))
					  (push! foxgl:polygon-cache2 r)
                                        ;(register-finalizer r cache-delete)
					  )
					
					(foxgl:color foxgl:current-color)
					(foxgl:transform foxgl:current-transform)
					(foxgl:blit-polygon (cdr r))
					
					
					)))))
    (hidden
     (set! model nil)))
  (when model
    (plist-rest (cdr model) foxgl:render-model2))  
  )

(defvar clone-mat4-identity (mat4-identity)) 
(defun clone-mat4(mat)
  (math:* clone-mat4-identity mat))

(defun foxgl:build-sub-models (model)
  (plist-rest model foxgl:build-physics))

(defvar physics-out '())

(defmacro foxgl:build-physics0(model)
  `(let ((cscope foxgl:current-scope))
     (set! foxgl:current-scope (lisp:get-current-scope))
     (foxgl:build-physics ,model)
     (set! foxgl:current-scope cscope)
     (swap physics-out nil)
     ))

(defun foxgl:build-physics (model)
  (case (car model)
    (aabb
     (push! physics-out (cons (clone-mat4 foxgl:current-transform) model))
     )
    (sphere
     (push! physics-out (cons (clone-mat4 foxgl:current-transform) model))
     )
    (capsule
     (push! physics-out (cons (clone-mat4 foxgl:current-transform) model))
     )
    (rgb )
    (color )
    (measure-model )
    (ref (foxgl:build-physics (symbol-value  (unbind2 (cadr model)))))
    (bind (foxgl:build-physics (eval (cadr model) foxgl:current-scope)))
    (for
     (let* ((var (cadr model))
            (evalues (or (unbind2 (caddr model)) nil))
            (rest (cdddr model))
            (prev-scope foxgl:current-scope))
       (lisp:with-scope-variable prev-scope (lisp:get-current-scope!!) '(evalues rest var) var
											'(
											  (set! foxgl:current-scope (lisp:get-current-scope!!))
											  (loop evalues
													 (lisp:scope-set! foxgl:current-scope var (unbind2 (car evalues)))
													 (foxgl:build-sub-models rest)
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
                                     
                                     (foxgl:build-sub-models body)

												 ))
			 
			 
			 (set! model nil)
			 (set! foxgl:current-scope prev-scope)))
    
    (view
     (let ((prev-transform foxgl:current-transform))
       (match p (unbind2 (plookup (cdr model) :perspective))
              (let ((fov (unbind2 (car p)))
                    (aspect (unbind2 (cadr p)))
                    (near (caddr p))
                    (far (cadddr p))
                    (prev-tform foxgl:current-transform))
                (set! foxgl:current-transform (mat4:perspective fov aspect near far))
                ))
       (match p (plookup (cdr model) :orthographic)
              (let ((w (unbind2 (car p)))
                    (h (unbind2 (cadr p)))
                    (z (unbind2 (caddr p)))
                    (prev-tform foxgl:current-transform))
                (set! foxgl:current-transform (mat4:orthographic w h z))
                ))
       (foxgl:build-sub-models (cdr model))
       (set! model nil)
       (set! foxgl:current-transform prev-transform)
       ))
    (rotate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (if (cons? rot)
           (math:rotate!  new-transform
                          (unbind2 (car rot))
                          (or (unbind2 (cadr rot)) 0.0)
                          (or (unbind2 (caddr rot)) 0.0))
           (math:rotate!  new-transform
                          0.0
                          0.0
                          (unbind2 rot))
           )
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (translate
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (math:translate! new-transform
                        (unbind2 (car rot))
                        (or (unbind2 (cadr rot)) 0.0)
                        (or (unbind2 (caddr rot)) 0.0))
       
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-models (cddr model))
       (set! foxgl:current-transform prev-tform)
       (set! model nil)
       (release-matrix new-transform)
       ))
    (scale
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (rot (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (if (list? rot)
           (math:scale! new-transform
                        (unbind2 (car rot))
                        (or (unbind2 (cadr rot)) 1.0)
                        (or (unbind2 (caddr rot)) 1.0))
           (math:scale! new-transform
                        rot rot rot))
       
       
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       )) 
    
    (skew
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix))
           (skew (unbind2 (cadr model))))
       (math:*! new-transform new-transform foxgl:current-transform)
       (math:skew! new-transform skew)
													 ;(println skew)
       
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-models (cddr model))
       (set! model nil)
       (set! foxgl:current-transform prev-tform)
       (release-matrix new-transform)
       ))
    (transform
     (let ((prev-tform foxgl:current-transform)
           (new-transform (get-matrix)))
       (when foxgl:current-transform
         (math:*! new-transform new-transform foxgl:current-transform))
       
       (match tlate (unbind2 (plookup (cdr model) ':translate))
              (math:translate! new-transform
                               (unbind2 (car tlate))
                               (unbind2 (cadr tlate))
                               (or (unbind2 (caddr tlate)) 0.0)
                               ))
       
       
       (match scale (unbind2 (plookup (cdr model) ':scale))
              (math:scale! new-transform 
                           (car scale) (cadr scale) (or (caddr scale) 1.0)))
       
       (match rotation (unbind2 (plookup (cdr model) ':rotate))
              
              (math:rotate!  new-transform
                             (unbind2 (car rotation))
                             (or (unbind2 (cadr rotation)) 0.0)
                             (or (unbind2 (caddr rotation)) 0.0)))
       (set! foxgl:current-transform new-transform)
       (foxgl:build-sub-models (cdr model))
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
  (when model
    (plist-rest (cdr model) foxgl:build-physics))  
  )

(load "sdf.lisp")

(let ((m1 (mat4-translation 4 0 0))
      (m2 (mat4-translation 3 2 1)))
  (mat4:print m1)
  (println "")
  (mat4:print m2)
  (println "")

  (mat4:print (math:* m2 m1))
  (println "")
  (println "")
  )

(println (math:* (mat3-scale 2 3) (mat3-scale 4 5)))
(println lisp:*web-environment*)
(unless (or t lisp:*web-environment*)
  (thread:join (thread:start (lambda () (println 'thread!))))
  (thread:join (thread:start (lambda () (println 'thread!))))
  
  (let ((srv (tcp:listen 8893))
        (cli (tcp:connect "127.0.0.1" 8893)))
    (let ((cli2 (tcp:accept srv))
			 (v (make-vector 4 1))
          (v2 (make-vector 10 (byte 0)))
			 )
      (vector-set! v 0 10101010101010)
      (fd:write cli2 v)
      (println (list 'read (fd:read cli v2)))
      (println (list srv cli cli2 v2))
      
      (fd:close cli2)
      (fd:close cli)
      (fd:close srv)
      ))
  )

(defvar audio:note-low (* 12.0 8.0))
(defun audio:note-to-frequency(note)
  (* 440.0 (math:pow 2.0 (/ (rational note) 12.0)))
  )

(defun process-song(song buffer sample-rate phase speed)
  (let ((i 0)
        (sample (rational sample-rate))
        (llen (vector-length buffer))
        (phase-incr (/ 1.0 phase)))
    
    (defun rec(song)
      
      (let ((fst (car song))
            (result 0.0))
        (when (eq fst 'melody)
          (let ((lst (cdr song))
                (p phase))
            (loop lst
                 (if (< p 0.25)
                     (progn ;; found note
                       (set! result (sin (* 6.28 (* p (audio:note-to-frequency (car lst))))))          (set! lst nil)
                       
                       )
                     (progn
                       (set! lst (cdr lst))
                       (set! p (- p 0.25)))))
            )
          )
        result
        ))
    (loop (< i llen)
       (vector-set! buffer i (float32 (rec song)))
       (incf phase phase-incr)
       (incf i 1))
    ))


(define foxgl:key-up 265)
(define foxgl:key-left 263)
(define foxgl:key-right 262)
(define foxgl:key-down 264)
(define foxgl:key-w 87)
(define foxgl:key-a 65)
(define foxgl:key-s 83)
(define foxgl:key-d 68)
(define foxgl:key-g 71)

(define foxgl:key-i 73)
(define foxgl:key-space 32)
(define foxgl:mappings '((up 264)
                         (down 265)))


(define window-title "window")

(defun eval-scoped02(scope form)
  (let ((head (car form)))
	 (if (symbol? head)
		  (let ((sub (symbol-value head scope)))
			 
			 (if (procedure? sub)
				  ;; if it is a function evaluate it as a function with scope.
				  (sub scope (cdr form))
				  (if (cons? sub)
						;; if it is a cons evaluate it as a 'scope' function.
						(let ((prevargs (symbol-value 'args scope)))
						  (lisp:scope-set! scope 'args (unbind (cdr form) scope))
						  (eval-scoped0 scope sub)
						  (lisp:scope-set! scope 'args prevargs)
						  )
						;; it is nothing special - default to recursing.
						(eval-scoped scope (cdr form))
		  				)))
		  ;; default to recursing.
		  (eval-scoped scope form)
		  )))

(defun eval-scoped2(scope body2)
  (for-each i body2
				(eval-scoped0 scope i)))

(defun scope:let(scope body)
  (let ((vars (car body))
		  (body (cdr body)))
	 (lisp:with-scope-binding scope (lisp:get-current-scope) '(body) vars
                             '((eval-scoped (lisp:get-current-scope) body)))))
(defun scope:for (scope body)
  (let ((var (car body))
		  (evalues (unbind (cadr body) scope))
		  (rest (cddr body)))
	 (if (eq (car evalues) 'range)
		  (let ((a (unbind (cadr evalues)))
				  (b (unbind (caddr evalues)))
				  (c (unbind (cadddr evalues))))
			 (let ((start (if b a 0))
					 (end (or c b a 1))
					 (step (if c b 1)))
				(unless (eq start end)
				(lisp:with-scope-variable
				  scope (lisp:get-current-scope!!)
				  '(start step end rest var) var
				  '((loop start
							 (lisp:scope-set! (lisp:get-current-scope!!) var start)
							 (eval-scoped (lisp:get-current-scope!!) rest)
							 (set! start (+ start step))
							 (if (>= start end)
								  (set! start nil))
							 ))))))
		  
	 (lisp:with-scope-variable
		  scope (lisp:get-current-scope!!)
		  '(evalues rest var) var
		  '((loop evalues
					(lisp:scope-set! (lisp:get-current-scope!!) var (unbind (car evalues) (lisp:get-current-scope!!)))
					(eval-scoped (lisp:get-current-scope!!) rest)
					(set! evalues (cdr evalues))))				 
		  ))))

(defun scope:for-table (scope body)
  (let ((scope:for-table:table (car body))
		  (rest (cdr body)))

	 (lisp:with-scope-variable
		  scope (lisp:get-current-scope!!)
		  '(scope:for-table:table rest) nil
		'((table:iter scope:for-table:table
			(eval-scoped (lisp:get-current-scope!!) rest)
			  )))))


(defun scope:if0 (scope body)
  (let ((r (eval (car body) scope)))
	 (when r
		(eval-scoped scope (cdr body))
		)))
		  
(defun scope:ref (scope body)
  (let ((ref (car body)))
	 (eval-scoped0 scope (symbol-value ref scope))))

(defun scope:bind (scope form)
  (let ((bind-clause (car form)))
	 (eval-scoped scope (eval bind-clause scope))))

(defun scope:bind2 (scope form)
  (let ((bind-clause (car form)))
	 (eval-scoped0 scope (eval (eval bind-clause scope) scope))))

(defvar base-scope
  (let ((vars scope:let)
		  (print (lambda (scope body) (println (unbind (car body) scope))))
		  (for scope:for)
		  (for-table scope:for-table)
		  (ref scope:ref)
		  (scope:if scope:if0)
		  (bind scope:bind)
		  (bind2 scope:bind2)
		  (args ())
		  (ignore (lambda (s b) ))
		  )
	 
	 (lisp:get-current-scope)))

(defun scope:transform (scope body)
  (let ((t0 (symbol-value 'current-transform scope))
		  (tform (unbind (car body) scope))
		  (rest (cdr body))
		  (current-transform (get-matrix)))
	 (math:*! current-transform t0 tform )
	 
	 (lisp:with-scope-binding scope
		(lisp:get-current-scope!!)
		'(current-transform rest) nil
		'((eval-scoped (lisp:get-current-scope!!) rest)))

	 (release-matrix current-transform)
	 ))

(defun scope:translate (scope body)
  (let ((t0 (symbol-value 'current-transform scope))
		  (translate (unbind (car body) scope))
		  (rest (cdr body))
		  (current-transform (get-matrix)))
	 (math:*! current-transform current-transform t0)
	 
	 (math:translate! current-transform
                     (unbind (car translate) scope)
                     (or (unbind (cadr translate) scope) 0.0)
                     (or (unbind (caddr translate) scope) 0.0))
	 
	 (lisp:with-scope-binding scope
		(lisp:get-current-scope!!)
		'(current-transform rest) nil
		'((eval-scoped (lisp:get-current-scope!!) rest)))

	 (release-matrix current-transform)
	 ))
(defvar rotate:pi (/ (* 2.0 pi) 360.0)) 
(defun scope:rotate (scope body)
  (let ((t0 (symbol-value 'current-transform scope))
		  (rot (unbind (car body) scope))
		  (rest (cdr body))
		  (current-transform (get-matrix)))
	 (math:*! current-transform current-transform t0)
	 (math:rotate! current-transform
                  (* rotate:pi (unbind (car rot) scope))
                  (* rotate:pi (or (unbind (cadr rot) scope) 0.0))
                  (* rotate:pi (or (unbind (caddr rot) scope) 0.0)))
	 (lisp:with-scope-binding scope
		(lisp:get-current-scope!!)
		'(current-transform rest) nil
		'((eval-scoped (lisp:get-current-scope!!) rest)))	
	 (release-matrix current-transform)
	 ))

(defun scope:scale (scope body)
  (let ((t0 (symbol-value 'current-transform scope))
		  (scale (unbind (car body) scope))
		  (rest (cdr body))
		  (current-transform (get-matrix)))
	 (math:*! current-transform current-transform t0)
	 (if (number? scale)
		  (math:scale! current-transform scale scale scale)
	 
		  (math:scale! current-transform
							(or (unbind (car scale) scope) 1.0)
							(or (unbind (cadr scale) scope) 1.0)
							(or (unbind (caddr scale) scope) 1.0)))
	 (lisp:with-scope-binding scope
		(lisp:get-current-scope!!)
		'(current-transform rest) nil
		'((eval-scoped (lisp:get-current-scope!!) rest)))	
	 (release-matrix current-transform)
	 ))

(defun scope:skew (scope body)
  (let ((t0 (symbol-value 'current-transform scope))
		  (skew (unbind (car body) scope))
		  (rest (cdr body))
		  (current-transform (get-matrix)))
	 (math:*! current-transform current-transform t0)
	 (math:skew! current-transform skew)
	 (lisp:with-scope-binding scope
		(lisp:get-current-scope!!)
		'(current-transform rest) nil
		'((eval-scoped (lisp:get-current-scope!!) rest))) 
	 (release-matrix current-transform)
	 ))



(defvar current-transform (mat4-identity))

(defvar transform-scope
  (eval
   '(let ((translate scope:translate)
			 (rotate scope:rotate)
			 (skew scope:skew)
			 (scale scope:scale)
			 (transform scope:transform)
			 (offset scope:translate)
			 ) 
	  (lisp:get-current-scope))
   base-scope))

(defun on-box (scope w h d)

  )

(defun scope:box (scope  model)
  (let ((w (unbind (car model) scope))
		  (h (unbind (cadr model) scope))
		  (d (unbind (caddr model) scope))
		  (on-box (symbol-value 'on-box scope)))
	 
	 (on-box scope w h d))
  )

													 ;(defvar bakery:out (list nil))

(defun bakery:push (scope data)
  (lisp:scope-set! scope 'bakery:out (cons data (symbol-value 'bakery:out scope))))

(defun bakery:sub (scope body)
  (let ((prev (symbol-value 'bakery:out scope)))
	 (lisp:scope-set! scope 'bakery:out nil)
	 (eval-scoped scope body)
	 (let ((new (symbol-value 'bakery:out scope)))
		(println (list 'new new))
		
		(lisp:scope-set! scope 'bakery:out prev)
		new)))

(defun bakery:box (scope w h d)
  (let((tform (symbol-value 'current-transform scope)))
	 (println (list '????? tform w h d))
	 
	 (bakery:push scope (list 'transform tform (list 'aabb w h d))
					  )))

(defun bakery:rgb (scope body)
  (let ((color (unbind (car body) scope))
		  (rest (cdr body))
		  )
	 (let ((color (foxgl:color->int
						(or (unbind (car color) scope) 0)
						(or (unbind (cadr color) scope) 0)
						(or (unbind (caddr color) scope) 0)
						(or (unbind (cadddr color) scope) 1.0)))
			 (sub (bakery:sub scope rest)))
		(println (list 'sub sub))
		(when sub
		  (bakery:push scope (cons 'rgb (cons color sub)))))	
	 ))

(defvar scope:bake-hashtable (make-hashtable))

(defun scope:bake (scope body)
  (let ((key (car body))
		  (dims (cadr body))
		  (rest (cddr body))
		  (on-box bakery:box)
		  (bakery:out nil)
		  (rgb bakery:rgb)
		  (current-transform (get-matrix))
		  
		  )
	 (let ((tex (hashtable-ref scope:bake-hashtable body)))

		(unless tex
		  (println 'baking)
		  (set! bakery:out (lisp:with-scope-binding scope
									(lisp:get-current-scope!!)
									'(on-box rest bakery:out rgb current-transform) nil
									'((eval-scoped (lisp:get-current-scope!!) rest)
									  bakery:out)))
		  (release-matrix current-transform)
		  
		  (let ((image (sdf:to-image bakery:out
											  (mat4-identity) scope:view-transform
											  (car dims) (cadr dims) (caddr dims))))
			 (set! tex (foxgl:load-texture-from-vector (car image) (car dims) (cadr dims) 3 ))
			 (hashtable-set scope:bake-hashtable body tex)))
													 ;(println foxgl:square-buffers)
		(unless foxgl:square-buffers
        
		  (set! foxgl:square-buffers
				  (list
					(foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))
					(foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))))
        
		  )
		(foxgl:bind-texture tex)
		(foxgl:color '(1.0 1.0 1.0 1.0))
													 ;(foxgl:transform mat2);scope:cache-scale)
		(foxgl:blit-mode :triangle-strip-depth)
		(foxgl:blit-polygon foxgl:square-buffers)
      (foxgl:bind-texture nil)
      (foxgl:blit-mode :triangle-strip)
													 ;(release-matrix mat2)
		
													 ;(println tex)

		)))


(defvar scope:color 0)
(defun scope:rgb (scope body)
  (let ((color (unbind (car body) scope))
		  (rest (cdr body))
		  (prev-color scope:color))
	 (set! scope:color
			 (foxgl:color->int (or (unbind (car color) scope) 0)
									 (or (unbind (cadr color) scope) 0)
									 (or (unbind (caddr color) scope) 0)
									 (or (unbind (cadddr color) scope) 1.0)))
	 (eval-scoped scope rest)
	 (set! scope:color prev-color)
	 ))

(defvar scope:view-transform (mat4:orthographic 1.0 1.0 1.0))

(defun scope:ortho (scope body)
  (let ((dims (unbind (car body) scope))
		  (rest (cdr body))
		  (prev current-transform))
	 
	 (set! current-transform
			 (mat4:orthographic (unbind (car dims) scope)
									  (unbind (cadr dims) scope)
									  (unbind (caddr dims) scope)))
	 (eval-scoped scope rest)
	 (set! current-transform prev)))

(defun scope:perspective (scope body)
  (let ((dims (unbind (car body) scope))
		  (rest (cdr body))
		  (prev current-transform))
	 (let ((fov (unbind (car dims) scope))
			 (aspect (or (unbind (cadr dims) scope) 1.0))
			 (near (or (unbind (caddr dims) scope) 0.1))
			 (far (or (unbind (cadddr dims) scope) 1000.0)))
		(set! current-transform 
				(mat4:perspective fov aspect near far))
		(eval-scoped scope rest)
		(set! current-transform prev))))

(defvar scope:depth:state nil)
(defun scope:depth (scope body)
  (let ((prev scope:depth:state))
	 (foxgl:depth (car body))
	 (set! scope:depth:state (car body))
	 (eval-scoped scope (cdr body))  
	 (foxgl:depth prev)
	 (set! scope:depth:state prev)
  ))

(defun scope:blend (scope body)
  (foxgl:blend 1)
  (eval-scoped scope (cdr body))  
  (foxgl:blend nil)
  
  )


(defvar paint-scope
  (eval
   '(let ((box scope:box)
			 (bake scope:bake x y)
			 (rgb scope:rgb)
			 (ortho (lambda (x y)(scope:ortho x y )))
			 (perspective scope:perspective)
			 (depth scope:depth)
			 (blend scope:blend)
			 )
	  (lisp:get-current-scope))
   transform-scope))

(defvar foxgl:polygon-cache3 (make-hashtable :weak))
(defvar foxgl:polygon-type (typespec-new 'polygon))
(defun foxgl:polygon-type.destruct (poly)
  (println 'destructing-polygon (cdr poly))
  (let ((polygons (cdr poly)))
	 (loop polygons
			 (foxgl:delete-polygon (car polygons))
			 (set! polygons (cdr polygons))))
  )
(typespec-set-destruct! foxgl:polygon-type foxgl:polygon-type.destruct)

(defun scope:polygon (scope model)
  (let ((dims 2)
        (poly (plookup model :2d-triangle-strip))
		  (view scope:view-transform)
		  (model-transform (symbol-value 'current-transform scope))
		  (verbose (symbol-value 'verbose scope))
		  )
    (unless poly
      (set! poly (plookup model :3d-triangle-strip))
      (set! dims 3)
      )
    (when poly
		
		(let ((r (hashtable-ref foxgl:polygon-cache3 model)))
        (unless r
          (set! r (list foxgl:polygon-type (foxgl:load-polygon (list-to-array poly) dims)))
	
			 (println (cons '>>>>> (cons r model)))
		    (hashtable-set foxgl:polygon-cache3 model r)
          )
		  (foxgl:color scope:color)
        (foxgl:transform model-transform)
		  (foxgl:blit-polygon (cdr r))
        ))))


(define foxgl:sdf-build-lookup2 (make-hashtable :weak))

(defun scope:sdf (scope model)
  (let ((conf (car model))
		  (body (cdr model)))
	 (let ((size (plookup conf :size))
			 (resolution (plookup conf :resolution))
			 (clip (unbind (plookup conf :clip)))
			 (positions nil)
			 )
      (when clip
        (let ((sdf-build (hashtable-ref foxgl:sdf-build-lookup (cdr model))))
          (unless sdf-build
            (set! sdf-build (foxgl:build-sdf (cdr model) foxgl:current-scope)))
          (hashtable-set foxgl:sdf-build-lookup (cdr model) sdf-build)
          (sdf:render sdf-build foxgl:current-transform clip)
          ))
      (unless clip
        (set! foxgl:sdf-stack nil)
		  (let ((r (hashtable-ref foxgl:polygon-cache3 model)))
			 (unless r
				(let* ((model2 (foxgl:build-sdf (cdr model) foxgl:current-scope) )
						 
						 (poly
                    (foxgl:sdf-marching-cubes size resolution model2 position)
							))
				  (set! r (list foxgl:polygon-type
									 (foxgl:load-polygon (car poly) 3 nil nil)
									 (foxgl:load-polygon (cdr poly) 3 nil nil)
													 ))
				  (hashtable-set foxgl:polygon-cache3 model r)
				  
				  ))
			 
			 (foxgl:color foxgl:current-color)
			 (foxgl:transform foxgl:current-transform)
			 (foxgl:blit-mode :triangles-color)
			 (foxgl:blit-polygon (cdr r))
			 (foxgl:blit-mode :triangle-strip)
			 
			 ))
      ))
  )

(defun scope:height-sdf (scope model)
  (let ((points (car model))
		  (resolution (cadr model))
		  (body (caddr model)))
	 (set! foxgl:sdf-stack nil)
	 (let ((r (hashtable-ref foxgl:polygon-cache3 model)))
		(unless r
		  (println (list 'getting-heightmap body) (foxgl:build-sdf body foxgl:current-scope))
		  (let* ((model2 (foxgl:build-sdf body foxgl:current-scope))
					
					(poly
                (sdf:height-map points resolution (cdr model2))
					  ))
			 (println poly)
			 (set! r (list foxgl:polygon-type
								;(foxgl:load-polygon (car poly) 3 nil nil)
								;(foxgl:load-polygon (cdr poly) 3 nil nil)
								))
			 ;(hashtable-set foxgl:polygon-cache3 model r)

			 
			 ))
		
		;(foxgl:color foxgl:current-color)
		;(foxgl:transform foxgl:current-transform)
		;(foxgl:blit-mode :triangles-color)
		;(foxgl:blit-polygon (cdr r))
		;(foxgl:blit-mode :triangle-strip)
			 
		)))


(defun scope:sdf0 (scope model)
  (let ((conf (car model))
		  (body (unbind (cadr model)))
		  (r (hashtable-ref foxgl:polygon-cache model))
		  (model-transform (symbol-value 'current-transform scope))
		  )
	 (unless r
		(let ((size (plookup conf :size))
				(resolution (plookup conf :resolution))
				(clip (unbind (plookup conf :clip)))
				)
		  (let* ((model2 body)
					(poly
					 (foxgl:sdf-marching-cubes size resolution model2 nil)
					  ))
			 (set! r (cons 'poly (list (foxgl:load-polygon (car poly) 3 nil nil)
												(foxgl:load-polygon (cdr poly) 3 nil nil)
												)))
          (hashtable-set foxgl:polygon-cache model r)
			 )))
	 (foxgl:color scope:color)
    (foxgl:transform model-transform)
    (foxgl:blit-mode :triangles-color)
    (foxgl:blit-polygon (cdr r))
    (foxgl:blit-mode :triangle-strip)
    )
  )

(defvar scope:cache-scale (mat4-identity))
(math:scale! scope:cache-scale 2 2 1)
(math:translate! scope:cache-scale -0.5 -0.5 0)
(defvar scope:fb-extra-info (make-hashtable))
(defun scope:cache-image(scope model)

  ;; in this test I should try projecting a unit vector space
  ;; into the current transform matrix. The angles and sizes should
  ;; be used for the key, otherwise rotated objects will be cached as
  ;; non-rotated.
  
  (let* ((tform (symbol-value 'current-transform scope))
			(eigen-key (foxgl:get-eigen-key tform))
			(key2 (unbind (plookup model :key) scope))
			(key (or key2 eigen-key))
			(fb (foxgl:get-framebuffer model key)))

	 (when (null? fb)
      (let ((s (plookup model :size)))
		  (set! fb (foxgl:load-framebuffer model (or s '(200 200)) key))
		  )
		;; the size is whatever.
		(foxgl:bind-framebuffer fb)
		(let ((submodel model))
		  (lisp:with-scope-binding scope
			 (lisp:get-current-scope!!)
			 '(submodel verbose) nil
			 '((eval-scoped (lisp:get-current-scope!!) submodel)))
		  
		  )
		(let ((iv (mat4:invert tform)))
		  (println iv)
		  (hashtable-set scope:fb-extra-info fb iv)) 
		(foxgl:unbind-framebuffer fb))

	 (unless foxgl:square-buffers
      
      (set! foxgl:square-buffers
            (list
             (foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))
             (foxgl:load-polygon (list-to-array '(0 0 1 0 0 1 1 1)))))
      )

	 (let ((itform (hashtable-ref scope:fb-extra-info fb)))
		(let ((mat2 (get-matrix)))
		  (math:*! mat2 mat2 tform)
		  (math:*! mat2 mat2 itform)
		  (math:*! mat2 mat2 scope:cache-scale)
		  
		  
		  (foxgl:bind-textures (foxgl:framebuffer-texture fb)
									  (foxgl:framebuffer-depth-texture fb)
									  ) 
		  (foxgl:color '(1.0 1.0 1.0 1.0))
		  (foxgl:transform mat2)
		  (foxgl:blit-mode :triangle-strip-depth)
		  (foxgl:blit-polygon foxgl:square-buffers)
		  (foxgl:bind-texture nil)
		  (foxgl:blit-mode :triangle-strip)
		  (release-matrix mat2)

		  ))))

(defvar scope:bake-cache (make-hashtable))
(defvar scope:bake-key-caches (make-hashtable))
(defun scope:bake2 (scope model)
  (let ((key (and (eq (car model) :key) (unbind (cadr model) scope)))
		  (cache scope:bake-cache)
		  )
	 (when key
		(set! cache (hashtable-ref scope:bake-key-caches model))
		(unless cache
		  (set! cache (make-hashtable))
		  (hashtable-set! scope:bake-key-caches model cache)) 
		(set! model (cddr model)))
	 (unless key
		(set! key model))
			 
  (let ((r (hashtable-ref cache key))
		  (model-transform0 (symbol-value 'current-transform scope))
		  )
    (unless r
		
		(let* ((baking-stack nil)
				 (polygon (lambda (scope model2)
								(let ((model-transform (symbol-value 'current-transform scope))
										(dims 2)
										(poly (plookup model2 :2d-triangle-strip)))
								  
								  (unless poly
									 (set! poly (plookup model2 :3d-triangle-strip))
									 (set! dims 3)
									 )
								  
								  (push! baking-stack
											(list scope:color
													(clone-mat4 model-transform)
													(list-to-array poly) dims))
								  
								  )))
				 ;; disable sub-level baking
				 ;; reusing the sub-level baking might make things faster
				 ;; but this is easier.
				 (bake2 nil)
				 (submodel model))
		  (lisp:with-scope-binding scope (lisp:get-current-scope!!)
											'(polygon submodel bake2) nil
											'((eval-scoped0 (lisp:get-current-scope!!) submodel)) 
											)
		  (let ((baked (foxgl:bake baking-stack model-transform0)))
          (set! r (list foxgl:polygon-type
								(foxgl:load-polygon (car baked) 3 nil nil)
                        (foxgl:load-polygon (cdr baked) 3 nil nil)
                        ))
		
			 (hashtable-set cache key r)
			 )
		  )
		)

	 (foxgl:color '(1 1 1 1))
    (foxgl:transform model-transform0)
	 (foxgl:blit-mode :triangle-strip-color)
    (foxgl:blit-polygon (cdr r))
    (foxgl:blit-mode nil)
    
	 )))

(defun scope:blit-text (scope model)
  (let ((model-transform (symbol-value 'current-transform scope))
		  (max-width (or (symbol-value 'max-width scope) 100.0)))
				 
	 (foxgl:color scope:color)
	 (foxgl:transform model-transform)
	 (foxgl:blit-text (unbind (car model) scope) model-transform max-width)
	 ))

(defun ui:measure-polygon (scope model)
  
  )
(defvar ui:desired-size (cons 0.0 0.0))

(defun ui:measure-text-base (scope model)
  (foxgl:measure-text (unbind (car model) scope) ui:desired-size))

(defun ui:measure-grid (scope model)
  (let ((desired-w 0.0)
		  (desired-h 0.0))
	 (for-each sub model
				  (set-cons! ui:desired-size 0.0 0.0)
				  (eval-scoped0 scope sub)
				  (set! desired-w (max desired-w (car ui:desired-size)))
				  (set! desired-h (max desired-h (cdr ui:desired-size)))
				  )
	 (set-cons! ui:desired-size desired-w desired-h)
	 ))

(defvar measure-scope
  (eval
	'(let ((polygon ui:measure-polygon)
			 (grid ui:measure-grid)
			 (text-base ui:measure-text-base)
			 )
	  (lisp:get-current-scope))))


(defvar ui:grid:model nil)
(defun ui:grid (scope model)
  (let ((prevmodel ui:grid:model))
	 (set! ui:grid:model model)
	 (lisp:with-scope-binding scope measure-scope
									  '(polygon grid text-base) nil
									  '((ui:measure-grid (lisp:get-current-scope!!) ui:grid:model))
									  )
	 (set! ui:grid:model prevmodel)
	 )
  (eval-scoped scope model)
  )



(defvar scope-3d
  (eval
   '(let ((polygon scope:polygon)
			 (sdf2 scope:sdf0)
			 (height-sdf scope:height-sdf)
			 (cache scope:cache-image)
			 (bake2 (lambda (a b) (scope:bake2 a b)))
			 (text-base scope:blit-text)
			 (grid (lambda (a b) (ui:grid a b)))
			 )
	  (lisp:get-current-scope))
   paint-scope))
