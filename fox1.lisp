
(load "lisp1.lisp")
(load "foxgl.lisp")

(define win (create-window (integer 800) (integer 600)))
(set-title win "Hello Ducky 2")
(make-current win)

(load-font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" (integer 22))
(define pix (load-texture-from-path "../iron/duck.png"))
(define pix (load-texture-from-path "test.img.png"))

(define p 0.1)
(loop t
     (blit-begin 2)
     (blit-scale 2.0 2.0)
     (blit-translate -0.5 -0.5)
     (blit-rectangle 0.2 0.2 0.3 1.0)
     (blit-begin 5)
     (blit-translate 0.0 20.0)     
     (blit-begin 2)
     (blit-translate -0.1 -0.1);(* (sin p) 0.5) 0.2)
     (blit-scale 1.0 1.0)
     (blit-color 1.0 1.0 1.0 1.0)
     (blit-texture pix)
     (blit-quad)
     (blit-scale 0.01 0.01)
     (blit-translate -30.0 -20.0)
     (blit-text "It's a duck!")
     (set p (+ p 0.03))
     
     (swap win)
     (poll-events)
     )
