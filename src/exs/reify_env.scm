(define old-eval-var eval-var)

(set! eval-var
  (clambda (e r k)
      (if (eq? '_env e) (k r) (old-eval-var e r k))))
