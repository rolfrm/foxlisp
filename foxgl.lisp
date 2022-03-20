(define foxgl (load-lib "./libiron.so"))
(define foxgl2 (load-lib "./foxgl.so"))
(println (list "foxgl2: " foxgl2))
(define create-window (load-alien foxgl "gl_window_open" native-null-pointer (list (integer 1) (integer 1) )))
(define make-current (load-alien foxgl "gl_window_make_current" nil (list native-null-pointer)))
(define swap (load-alien foxgl "gl_window_swap" nil (list native-null-pointer)))
(define set-title (load-alien foxgl "gl_window_set_title" nil (list native-null-pointer "test")))

(defmacro foxglf (name sym ret &rest args)
  (list (quote define) name (list (quote load-alien) (quote foxgl) sym ret (append (quote list) args))))

(defmacro foxglf2 (name sym argcnt)
  (list (quote define) name (list (quote load-wrap) (quote foxgl2) sym ret (append (quote list) args))))
(foxglf blit-begin "blit_begin" nil 1)
(foxglf poll-events "gl_window_poll_events" nil)
(foxglf timestamp "timestamp" 0)
(define blit-rectangle (load-wrap foxgl2 "rect2" 4))
(define blit-scale (load-wrap foxgl2 "scale" 2))
(define blit-translate (load-wrap foxgl2 "translate" 2))
(define blit-color (load-wrap foxgl2 "color" 4))
(define blit-text (load-wrap foxgl2 "text" 1))
(define load-font (load-wrap foxgl2 "load_font" 2))
(define load-texture-from-path (load-wrap foxgl2 "load_texture_from_path" (integer 1)))
(define blit-texture (load-wrap foxgl2 "set_texture" 1))
(define blit-quad (load-wrap foxgl2 "quad"))

(define mat-mul (load-wrap foxgl2 "lmat4_mul" 2))
(define mat4-print (load-wrap foxgl2 "lmat4_print" 1))

(define blit3d-init (load-wrap foxgl2 "lblit_init" 0))
(define blit3d-square (load-wrap foxgl2 "lblit_square2" 0))
(define blit3d-color (load-wrap foxgl2 "lblit_color" 1))
(define blit3d-transform (load-wrap foxgl2 "lblit_transform" 1))

(define load-polygon (load-wrap foxgl2 "load_polygon" 1))
(define blit-polygon (load-wrap foxgl2 "blit_polygon" 1))
(define delete-polygon (load-wrap foxgl2 "delete_polygon" 1))

(define thread-start (load-wrap foxgl2 "thread_start" 1))
(define thread-join (load-wrap foxgl2 "thread_join" 1))
(define sleep (load-wrap foxgl2 "lisp_sleep" 1))

(define tcp-listen (load-wrap foxgl2 "tcp_listen" 1))
(define tcp-accept (load-wrap foxgl2 "tcp_accept" 1))
(define tcp-connect (load-wrap foxgl2 "tcp_connect" 2))

(define close (load-wrap foxgl2 "lisp_close" 1))
(define read (load-wrap foxgl2 "lisp_read" 2))
(define write (load-wrap foxgl2 "lisp_write" 2))

(define gllib (load-lib "libGL.so"))
(define glerror (load-alien gllib "glGetError" 1))


(assert load-font "!!")
(define build-distance-field (load-wrap foxgl2 "lisp_build_df" 1))
(define distance-field-print (load-wrap foxgl2 "lisp_print_df" 1))
(define blit-distance-field (load-wrap foxgl2 "lisp_blit_distance_field" 4))

(define df1 (build-distance-field
             '
             (color :rgb (0.9 0.5 0.99)
                    (sphere :radius 2.0 :center (2.0 0.0 5.0))
                    (color :rgb (0.0 0.0 1.0)
                           (sphere :radius 1.0 :center (-2.0 -1.0 5.0)))
                    (color :rgb (0.0 1.0 0.0)
                           (sphere :radius 1.0 :center (-2.0 3.0 5.0)))
                    (color :rgb (0.0 0.0 0.0)
                           (triangle :a (-3.0 3.0 3.0) :b (3.0 -3.0 5.0) :c (3.0 3.0 10.0)))
                    (color :rgb (1.0 1.0 1.0)
                           (triangle :a (-0.1 -1.1 2.2) :b (-5.0 5.0 15.0) :c (3.0 -3.0 15.0)))
                    
                    )))

(define df '(color
             :rgb (1 0 1)
             (sphere :center (1 1 1) :radius 1.0)))
(println (list 'df1 df1))
(distance-field-print df1) (println " ")
(let ((t1 (timestamp)))
  (blit-distance-field nil 64 64 df1)
  (println `(rendering took ,(- (timestamp) t1) us)))
(println (type-of (byte 1)))
(println (timestamp))

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
    (vector-set! m 0 (float32 1.0))
    (vector-set! m 5 (float32 1.0))
    (vector-set! m 10 (float32 1.0))
    (vector-set! m 15 (float32 1.0))
    m
    ))

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

(define render-model nil)

(let ((color '(1 1 1 1))
      (transform nil)
      (polygon-cache (make-hashtable t nil))
      (cache-delete (lambda (x)
                      (println 'delete-poly)
                      (delete-polygon (cdr x))))
      )
  (set!
   render-model
   (lambda (model)
     (blit3d-init)
     (let ((sym (car model))
           (funcs ())
           )
       (when (eq sym 'color)
         (let ((prev-color color))
           (push! funcs (lambda () (set! color prev-color)))
           )
         
         (let ((rgb (plookup (cdr model) ':rgb)))
           (set! color rgb)
           ))
       (when (eq sym 'transform)
         (let ((prev-tform transform))
           (push! funcs (lambda () (set! transform prev-tform))))
         (match tlate (plookup (cdr model) ':translate)
                (set! transform (mat-mul (or transform (mat4-identity))
                                         (mat4-translation (car tlate) (cadr tlate) (or (caddr tlate) 0.0)))))
         (match scale (plookup (cdr model) ':scale)
                
                (set! transform (mat-mul (or transform (mat4-identity))
                                         (mat4-scale (car scale) (cadr scale) (or (caddr scale) 1.0))))
                
                ))
       (when (eq sym 'unit-square)
         (blit3d-color color)
         (blit3d-transform (or transform (mat4-identity)))
         (blit3d-square)
         )
      (when (eq sym 'print)
        (println (list (cdr model) transform color)))
       (when (eq sym 'polygon)
         (match poly (plookup (cdr model) ':2d-triangle-strip)
                (let ((r (hashtable-ref polygon-cache (cdr model))))
                  
                  (unless r
                    (println (list 'new-poly r))
                    (set! r (cons 'poly (load-polygon (list-to-array poly))))
                    (hashtable-set polygon-cache (cdr model) r)
                    ;(register-finalizer r cache-delete)
                    )
                  (blit3d-color color)
                  (blit3d-transform (or transform (mat4-identity)))
                  (blit-polygon (cdr r))
                  
                  )
                
                ))
       
       (plist-rest (cdr model) render-model)
       
       (map! funcall funcs)  
       ))))

(let ((m1 (mat4-translation 4 0 0))
      (m2 (mat4-translation 3 2 1)))
  (mat4-print m1)
  (println "")
  (mat4-print m2)
  (println "")

  (mat4-print (mat-mul m2 m1))
  (println "")
  (println "")
  )

(println (mat-mul (mat3-scale 2 3) (mat3-scale 4 5)))

(thread-join (thread-start (lambda () (println 'thread!))))
(thread-join (thread-start (lambda () (println 'thread!))))

(define add (lambda (a b) (+ a b)))
(defun add (a b) (+ a b))

(define (add a b) (+ a b))

(let ((srv (tcp-listen 8893))
      (cli (tcp-connect "127.0.0.1" 8893)))
  (let ((cli2 (tcp-accept srv))
        (v (make-vector 4 1))
        (v2 (make-vector 10 (byte 0)))
        )
    (vector-set! v 0 10101010101010)
    (write cli2 v)
    (println (list 'read (read cli v2)))
    (println (list srv cli cli2 v2))
    
    (close cli2)
    (close cli)
    (close srv)
    ))

(load "swank.lisp")
