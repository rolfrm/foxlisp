;; open issue: macro name the same as the first argument name.
(def x (macro (&rest b) b))
(def add (lambda (x y) (+ x y)))
(println (add 123 321))
