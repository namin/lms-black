;; at EM EM
;; load reify_env

;; at EM
(define list
  (lambda args args))

(define old-eval-application eval-application)

(define delta?
  (lambda (e)
    (if (pair? e) (if (pair? (car e)) (eq? 'delta (car (car e))) #f) #f)))

(define id-cont
  (lambda (v) v))

(define make-pairs
  (lambda (ks vs) 'TODO))
(set! make-pairs
  (lambda (ks vs)
      (if (null? ks) '()
          (if (null? vs) '()
              (if (symbol? ks) (cons (cons ks vs) '())
                  (cons (cons (car ks) (car vs)) (make-pairs (cdr ks) (cdr vs))))))))

(define extend
  (lambda (env params args)
      (cons (make-pairs params args) env)))

(define apply-delta
  (lambda (e r k)
    (let ((operator (car e))
          (operand (cdr e)))
      (let ((delta-params (car (cdr operator)))
            (delta-body (cdr (cdr operator))))
        (eval-begin
         delta-body
         (extend _env delta-params (list operand r k))
         id-cont)))))

(set! eval-application
      (lambda (e r k)
        (if (delta? e)
            (apply-delta e r k)
            (old-eval-application e r k))))

(define meaning base-eval)

;; at user level

(define map
  (lambda (f xs)
    (if (null? xs)
        '()
        (cons (f (car xs)) (map f (cdr xs))))))

(let ((x 1)) ((delta (e r k) (meaning (car e) r k)) x))
;; => 1

(define call/cc
  (lambda (f)
    ((delta (e r k)
      ((meaning 'f r (lambda (v) v)) k)))))

(+ 1 (call/cc (lambda (k) 0)))
;; => 0

(+ 1 (call/cc (lambda (k) (k 0))))
;; => 1
