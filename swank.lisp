;;; for interacting with lisp interaction
;;; from https://github.com/nickg/swank-chicken/blob/master/swank-chicken.scm
(defun current-process-id ()
  1)

(define swank:coding-system "utf-8-unix")

(defun swank:connection-info ()
  `(:ok (:pid ,(current-process-id)
         :package (:name CSI :prompt CSI)
         :encoding (:coding-systems (,swank:coding-system))
         :lisp-implementation 
         (:type "Fox Lisp" :version "0.1"))))

(defun swank:swank-require (&rest args)
  '(:ok nil))

(defun swank:buffer-first-change (&rest args) '(:ok nil))
(defun swank:filename-to-modulename (&rest args) '(:ok nil))
(defun swank:find-definitions-for-emacs (&rest args)  '(:ok nil))
(defun swank:init-presentations (&rest args) '(:ok nil))
(defun swank:create-repl (&rest args)
  '(:ok ("CSI" "CSI")))
(defun swank-repl:create-repl (&rest args)
  '(:ok ("CSI" "CSI")))

(defun swank-repl:listener-eval (str )
  (let ((e (lambda ()
             (let ((forms (read-string str)))
               (if (not (null? forms))
                   (with-exception-handler
                       (eval `(progn ,forms))
                     (lambda (ex) ex))

                   )))))
    (let ((result (e)))
      `(:ok (:values ,(value->string result))))))

(defun swank:compile-string-for-emacs (str buffer position filename _)
  (let ((e (lambda ()
             (let ((forms (read-string str)))
               (if (not (null? forms))
                   (eval `(progn ,forms)))))))
    (let ((result (e)))
      (println result)
      `(:ok (:compilation-result nil t 0.0 nil nil)))))

(defun swank:completions (prefix _)
                                        ;(println (list 'completions prefix))
  (let ((matching (take (lambda (x) (string-starts-with (symbol->string x) prefix)) (all-symbols))))
    (println `(:ok (,(map matching symbol->string) ,(if (cdr matching) prefix (when matching (symbol->string (car matching)))))))))


(defun swank:autodoc (forms &rest args)
  ;(println forms)
                                        ;(println 'AUTODOC)
                                        ;(println (list forms args))
  
  (defun find-cursor(thing)
    (cond
      ((and (list? thing)
            (memq 'swank::%cursor-marker% thing))
       thing)
      ((list? thing)
       
       (let ((asd (lambda (elems)
                    (unless (null? elems)
                      (or (find-cursor (car elems))
                          (asd (cdr elems)))))))
         (asd thing)))
      (else #f)))
  
  (defun highlight-arg (info args)
                                        ;(println 'find-cursor)
    
    (cond
      ((null? info) '())
      ((null? args) info)
      ((not (pair? info))  ; Variable length argument list
       (list '===> info '<===))  
      ((eq? (car args) 'swank::%cursor-marker%)
       (append (list '===> (car info) '<===)
               (cdr info)))
      ((and (string? (car args)) (string=? (car args) ""))
       (highlight-arg info (cdr args)))
      (else (cons (car info)
                  (highlight-arg (cdr info) (cdr args))))))
  
  (defun info (sym)
                                        ;(println 'info)
    (cond
      ((not (bound? sym t)) #f)
      ((symbol-procedure? sym)
       (let ((pi (type-of (symbol-value sym))))
         (if (pair? pi)
             pi
             `(,pi . args))))
      (else #f)))

  ;; Choose a doc-node from all matches, this is only a heuristic solution.
  ;; The heuristic is to find the doc node that has the same name as the symbol,
  ;; and the module name is listed in ##sys#module-table. If there are multiple
  ;; such nodes, choose the first one.
  ;;
  (defun guess-doc-node(doc-nodes)
                                        ;(println 'doc-node)  
    (case (length doc-nodes)
      ((0) #f)
      ;; ((1) (car doc-nodes))
      (else (any (lambda (n) (and (assoc (car (chicken-doc#node-path n))
                                         ##sys#module-table)
                                  n))
                 doc-nodes))))
  (defun signature-from-doc (sym)
    
    (when (bound? sym t)
      (let ((signature (function-signature (eval sym)))
            (describe (lisp:describe (eval sym))))
        (or describe signature))))
  
  (let ((where (find-cursor forms)))
    (if (and where (string? (car where)) (not (string= (car where) "")))
        (progn
          (let* ((sym (string->symbol (car where)))
                 (i (or (signature-from-doc sym) (info sym))))
                                        ;(println 'oook)
            (if i
                `(:ok (,(value->string (highlight-arg i where)) t))
                `(:ok (:not-available t)))))
        '(:ok (:not-available t)))))


(defun swank-handle-command (slime cmd)
  (println (cons 'swank: cmd))
  (when (eq (car cmd) ':emacs-rex)
    
    (let ((str (value->string `(:return ,(eval (cadr cmd)) ,(last cmd)))))
      (println (list 'response str))
      (let ((strbuf (string->vector str)))
        (let ((lenbuf (hex-string (vector-length strbuf) 6)))
          (fd:write slime (string->vector lenbuf))
          (fd:write slime (string->vector str)))
        ))))

(define *swank-mutex* (thread:create-mutex))
(println *swank-mutex*)
(thread:lock-mutex *swank-mutex*)
(thread:unlock-mutex *swank-mutex*)
(assert *swank-mutex*)

(defun swank-event-loop(slime)
  
  (loop active
        (thread:lock-mutex *swank-mutex*)
        
        (let ((len-buf (make-vector 6 (byte 0)))
              (active t)
              )
          
          (let ((read-len (read slime len-buf)))
            (when (< 0 read-len)
              (progn
                (let ((len (parse-hex (vector->string len-buf))))
                  (let ((buf2 (make-vector len (byte 0))))
                    (read slime buf2)
                    (let ((cmd (read-string (vector->string buf2))))
                      (with-exception-handler
                          (swank-handle-command slime cmd)
                        (lambda (ex)
                          (println ex)))
                      )))
                )
              )))

        (thread:unlock-mutex *swank-mutex*)
        
        ))

(define *swank:server-port* 8810)

(defun swank-make-server (port file)
  (let ((listener (tcp:listen port)))
    (let ((emacs (tcp:accept listener)))
      (swank-event-loop emacs))))

(defun swank-server-new (port)
  (let ((listener (tcp:listen port)))
    (println "listening")
    (fd:set-blocking listener nil)
    (cons listener nil)))
                                        ;(let ((emacs (tcp:accept listener)))
                                        ;  (println "ok")
                                        ;  emacs)))
(defun swank-server-update(listener)
  (unless (cdr listener)
    (let ((new (tcp:accept (car listener))))
      (unless (eq (cdr new) -1)
        (fd:set-blocking new t)
        
        (set-cdr! listener new))))
  (when (cdr listener)
    (let ((len-buf (make-vector 6 (byte 0)))
          (slime (cdr listener))
          (next t)
          )
      (while next
             (let ((read-len (fd:recv slime len-buf)))
               (when (nil? read-len)
                 (set! read-len 0)
                 (fd:close slime)
                 (set-cdr! listener nil)
                 )
               (when (= 0 read-len)
                 (set! next nil))
               (when (< 0 read-len)
                 (let ((len (parse-hex (vector->string len-buf))))
                   (println (list 'read-len len))
                   (let ((buf2 (make-vector len (byte 0))))
                     (fd:read slime buf2)
                     (let ((cmd (read-string (vector->string buf2))))
                       (with-exception-handler
                           (swank-handle-command slime cmd)
                         (lambda (ex)
                           (println ex)))
                       )))
                 ))))))

(defun swank:start-server ()
  (swank-make-server *swank:server-port* nil))


(define swank-test '(("li" "CSI>" ("+" "1" "2") "3" "CSI>" ("println" "'123") "123" "CSI>" ("li" "CSI>" ("list?" ("asd")) "t" "CSI>" ("string->vector" ""asd"") ("97" "115" "100") "CSI>" ("string->vector" ""asd"") ("97" "115" "100") "CSI>" ("string->vector" "" swank::%cursor-marker%))) (:print-right-margin 104))
  )
(define swank-test-2 '((("defun" "swank:autodoc" ("forms" "&rest" "args") ("println" ("list" "forms" "args")) ("defun" "find-cursor" ("thing") ("cond" (("and" ("list?" "thing") ("memq" "'swank::%cursor-marker%" "thing")) "thing") (("list?" "thing") ("let" (("asd" ("lambda" ("elems") ("unless" ("null?" "elems") ("or" ("find-cursor" ("car" "elems")) ("asd" ("cdr" "elems"))))))) ("asd" "thing"))) ("else" "#f"))) ("defun" "highlight-arg" ("info" "args") ("cond" (("null?" "info") nil) (("null?" "args") "info") (("not" ("pair?" "info")) ("list" "'===>" "info" "'<===")) (("eq?" ("car" "args") "'swank::%cursor-marker%") ("append" ("list" "'===>" ("car" "info") "'<===") ("cdr" "info"))) (("and" ("string?" ("car" "args")) ("string=?" ("car" "args") """")) ("highlight-arg" "info" ("cdr" "args"))) ("else" ("cons" ("car" "info") ("highlight-arg" ("cdr" "info") ("cdr" "args")))))) ("defun" "info" ("sym") ("cond" (("unbound?" "sym") "#f") (("symbol-procedure?" "sym") ("let" (("pi" ("type-of" ("symbol-value" "sym")))) ("if" ("pair?" "pi") "pi" (",pi" "." "args")))) ("else" "#f"))) ("defun" "guess-doc-node" ("doc-nodes") ("println" ("list" "'info-about" "doc-nodes")) ("case" ("length" "doc-nodes") (("0") "#f") ("else" ("any" ("lambda" ("n") ("and" ("assoc" ("car" ("chicken-doc#node-path" "n")) "##sys#module-table") "n")) "doc-nodes")))) ("defun" "mega-consify" ("list") ("if" ("cddr" "list") ("cons" ("car" "list") ("mega-consify" ("cdr" "list"))) ("cons" ("car" "list") ("cadr" "list")))) ("defun" "signature-from-doc" ("sym") ("when" ("bound?" "sym") ("println" "'function-signature") ("println" ("function-signature" ("eval" "sym"))))) ""
                         (cond ((eq? sym 'define) ))
                         (if (or (symbol-macro? sym) (symbol-procedure? sym))
                             (let* ((doc-nodes nil);(match-nodes sym))
                                    (guessed-doc-node nil)); (guess-doc-node doc-nodes)))
                               (cond
                                 ((null? doc-nodes) #f)
                                 (guessed-doc-node
                                  (car (string->forms
                                        (string-delete (char-set #\[ #\])
                                                       (chicken-doc#node-signature guessed-doc-node)))))
                                 (else #f)))
                             #f))
                        "" ("let" (("where" ("find-cursor" swank::%cursor-marker%))))) (:print-right-margin 138)))


(println (list 'asd (function-signature swank-make-server)))

(println (swank:autodoc (car swank-test)))
(println (swank:autodoc (car swank-test-2)))

(define swank-test-3 '(("lisp-write-doc" "hashtable-set!" ("" swank::%cursor-marker%)) (:print-right-margin 181)))

(println (swank:autodoc (car swank-test-3)))

(define swank-test-4 '(swank:autodoc (quote ("define" "sample1" ("make-vector" swank::%cursor-marker%))) :print-right-margin 141))
;(swank:autodoc (car swank-test-4))
;(define swank-test-5 '("defun" "swank-event-loop" ("slime") ("let" (("len-buf" ("make-vector" "6" ("byte" "0"))) ("active" "t")) ("loop" "active" swank::%cursor-marker%))))

(println 'test-done)
