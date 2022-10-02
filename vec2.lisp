
(defun vec2(x y)
  (let ((v (make-vector 2 (float32 0.0))))
    (vector-set! v 0 (float32 x))
    (vector-set! v 1 (float32 y))
    v))
(defun vec2-x(v)
  (vector-ref v 0))
(defun vec2-y(v)
  (vector-ref v 1))
(defmacro vec2-f (name f)
  `(defun ,name (a b)
    (vec2
     (,f (vec2-x a) (vec2-x b))
     (,f (vec2-y a) (vec2-y b)))))

(vec2-f vec2+ +)
(vec2-f vec2- -)
(vec2-f vec2/ /)
(vec2-f vec2* *)

(defun vec2-scale (v s)
  (vec2* v (vec2 s s)))

(defun vec2-normalize(v)
  (vec2-scale v (/ (float32 1.0) (vec2:len v))))

(defun vec2-90(v)
  (vec2 (vec2-y v) (- 0.0 (vec2-x v))))

(defun vec2-dot(a b)
  (+ (* (vec2-x a) (vec2-x b))
     (* (vec2-y a) (vec2-y b))))
     

(defun list->vec2(lst)
  (vec2 (pop lst) (pop lst)))

(defun vec3(x y z)
  (let ((v (make-vector 3 (float32 0.0))))
    (vector-set! v 0 (float32 x))
    (vector-set! v 1 (float32 y))
    (vector-set! v 2 (float32 z))
    v))
(defun vec3-x(v)
  (vector-ref v 0))
(defun vec3-y(v)
  (vector-ref v 1))
(defun vec3-z(v)
  (vector-ref v 2))
(defmacro vec3-f (name f)
  `(defun ,name (a b)
    (vec3
     (,f (vec3-x a) (vec3-x b))
     (,f (vec3-y a) (vec3-y b))
     (,f (vec3-z a) (vec3-z b)))))

(vec3-f vec3+ +)
(vec3-f vec3- -)
(vec3-f vec3/ /)
(vec3-f vec3* *)
(defun vec3-len (a)
  (let ((x (vec3-x a)) (y (vec3-y a)) (z (vec3-z a)))
    (math:sqrtf (+ (+ (* x x) (* y y)) (* z z)))))

(defun vec3-scale (v s)
  (vec3* v (vec3 s s s)))

(defun vec3-normalize(v)
  (vec3-scale v (/ 1.0 (vec3-len v))))


(defun list->vec3(lst)
  (vec3 (pop lst) (pop lst) (pop lst)))

(defvar mat4:*identity* (mat4:rotate 0.0 0.0 0.0))

(defun vec3-rotation(v)
  (mat4:rotate (vec3-x v) (vec3-y v) (vec3-z v)))

(defun vec2-rotate(v r)
  (let* ((m (mat4:rotate 0.0 0 r))
         (v3 (vec3 (vec2-x v) (vec2-y v) 0.0))
         (v3r (math:* m v3)))
    (vec2 (vec3-x v3r) (vec3-y v3r))))

(println (vec2-rotate (vec2 1.0 2.0) 2.0))
