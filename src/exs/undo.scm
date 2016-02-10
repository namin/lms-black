;; at EM EM

;; load reify_env

(define undo-list '())

(define old-eval-set! eval-set!)

(define list
  (lambda args args))

(set! eval-set!
  (clambda (e r k)
      (let ((name (car (cdr e))))
        (eval-var name r (lambda (v)
                           (set! undo-list (cons (cons name v) undo-list))
                           (old-eval-set! e r k))))))

(define reflect-undo!
  (clambda (r)
      (if (null? undo-list)
          '()
          (begin
            (old-eval-set!
             (list 'set! (car (car undo-list)) (cons 'quote (cdr (car undo-list))))
             r
             (lambda (v) v))
            (set! undo-list (cdr undo-list))))))

;; at EM
(define undo!
  (clambda ()
      ((EM reflect-undo!) _env)))
