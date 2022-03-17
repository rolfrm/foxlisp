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

(define (swank:autodoc forms . args)

    (define (find-cursor thing)
    (cond
     ((and (list? thing)
           (memq 'swank::%cursor-marker% thing))
      thing)
     ((list? thing)
      (let loop ((elems thing))
        (if (null? elems)
            #f
            (or (find-cursor (car elems))
                (loop (cdr elems))))))
     (else #f)))
  
  (define (highlight-arg info args)
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

  (define (symbol-procedure? sym)
    (handle-exceptions exn #f (procedure? (eval sym))))
  
  (define (info sym)
    (cond
     ((unbound? sym) #f)
     ((symbol-procedure? sym)
      (let ((pi (procedure-information (symbol-value sym))))
        (if (pair? pi)
            pi
            `(,pi . args))))
     (else #f)))

  ;; Choose a doc-node from all matches, this is only a heuristic solution.
  ;; The heuristic is to find the doc node that has the same name as the symbol,
  ;; and the module name is listed in ##sys#module-table. If there are multiple
  ;; such nodes, choose the first one.
  ;;
  (define (guess-doc-node doc-nodes)
    (case (length doc-nodes)
      ((0) #f)
      ;; ((1) (car doc-nodes))
      (else (any (lambda (n) (and (assoc (car (chicken-doc#node-path n))
                                         ##sys#module-table)
                                  n))
                 doc-nodes))))

  (define (signature-from-doc sym)
    (cond ((eq? sym 'define) ))
    (if (or (##sys#macro? sym) (symbol-procedure? sym))
        (let* ((doc-nodes (match-nodes sym))
               (guessed-doc-node (guess-doc-node doc-nodes)))
          (cond
           ((null? doc-nodes) #f)
           (guessed-doc-node
            (car (string->forms
                  (string-delete (char-set #\[ #\])
                                 (chicken-doc#node-signature guessed-doc-node)))))
           (else #f)))
        #f))

  (let ((where (find-cursor forms)))
    (if (and where (string? (car where)))
        (let* ((sym (string->symbol (car where)))
               (i (or (signature-from-doc sym) (info sym))))
          (if i
              `(:ok (,(fmt #f (highlight-arg i where)) t))
              `(:ok (:not-available t))))
        '(:ok (:not-available t)))))

(defun last (lst)
  (if (cdr lst) (last (cdr lst)) (car lst)))

(defun swank-handle-command (slime cmd)
  (println (list 'command cmd))
  (when (eq (car cmd) ':emacs-rex)
    (let ((str (value->string `(:return ,(eval (cadr cmd)) ,(last cmd)))))
      (let ((strbuf (string->vector str)))
        (let ((lenbuf (hex-string (vector-length strbuf) 6)))
          (println (list 'write: str lenbuf))
      
          (write slime (string->vector lenbuf))
          (write slime (string->vector str)))
                                        ;(println (list slime cmd))
    ))))



(defun swank-event-loop(slime)
  (let ((len-buf (make-vector 6 (byte 0)))
        (active t)
        )
    (loop active
          (let ((read-len (read slime len-buf)))
            (sleep 0.25)
            (when (< 0 read-len)
              
            (let ((len (parse-hex (vector->string len-buf))))
              (let ((buf2 (make-vector len (byte 0))))
                (read slime buf2)
                (let ((cmd (read-string (println (vector->string buf2)))))
                  (swank-handle-command slime cmd)
                ))))))))

(defun swank-make-server (port file)
  (let ((listener (tcp-listen port)))
    (let ((emacs (tcp-accept listener)))
      (swank-event-loop emacs))))
(swank-make-server 8810 nil)