;; at EM EM
;; load reify_env

;; at EM
(define list
  (clambda args args))

(define old-eval-application eval-application)

(define delta?
  (clambda (e)
    (if (pair? e) (if (pair? (car e)) (eq? 'delta (car (car e))) #f) #f)))

(define id-cont
  (clambda (v) v))

(define make-pairs
  (clambda (ks vs) 'TODO))
(set! make-pairs
  (clambda (ks vs)
      (if (null? ks) '()
          (if (null? vs) '()
              (if (symbol? ks) (cons (cons ks vs) '())
                  (cons (cons (car ks) (car vs)) (make-pairs (cdr ks) (cdr vs))))))))

(define extend
  (clambda (env params args)
      (cons (make-pairs params args) env)))

(define apply-delta
  (clambda (e r k)
    (let ((operator (car e))
          (operand (cdr e)))
      (let ((delta-params (car (cdr operator)))
            (delta-body (cdr (cdr operator))))
        (eval-begin
         delta-body
         (extend _env delta-params (list operand r k))
         id-cont)))))

(set! eval-application
      (clambda (e r k)
        (if (delta? e)
            (apply-delta e r k)
            (old-eval-application e r k))))

(define meaning base-eval)

;; at user level

(define map
  (lambda (f xs) 'TODO))
(define map
  (clambda (f xs)
    (if (null? xs)
        '()
        (cons (f (car xs)) (map f (cdr xs))))))

(let ((x 1)) ((delta (e r k) (meaning (car e) r k)) x))
;; => 1

(define call/cc
  (lambda (f) ;; semantically, we cannot use a clambda here, because it resets the continuation
    ((delta (e r k)
      ((meaning 'f r (clambda (v) v)) k)))))

(+ 1 (call/cc (clambda (k) 0)))
;; => 0

(+ 1 (call/cc (clambda (k) (k 0))))
;; => 1
