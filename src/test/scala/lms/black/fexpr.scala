package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestFexpr extends TestSuite with BeforeAndAfter {
  val under = "fexpr_"

  before {
    clean()
  }

  def fexpr = """(EM
(set! eval-application
  (clambda (exp env cont)
    (if (eq? (car exp) 'fexpr)
       (base-eval (cons 'lambda (cdr exp)) env (lambda (clo) (cont (cons 'fexpr clo))))
       (base-eval
        (car exp) env
        (lambda (op)
          (if (if (pair? op) (eq? 'fexpr (car op)) #f)
             (base-apply (cons (cdr op) (cdr exp)) env cont)
             (eval-list (cdr exp) env (lambda (l) (base-apply (cons op l) env cont)))))))))
)"""

  def fexpr_compiled = fexpr.replace("'lambda", "'clambda").replace("(lambda", "(clambda")

  def top_eval = """(begin
(EM (begin
(define old-eval-var eval-var)

(set! eval-var
  (clambda (e r k)
      (if (eq? '_env e) (k r) (old-eval-var e r k))))
))
(define eval (lambda (exp) ((EM base-eval) exp _env (lambda (x) x))))
)"""

  test("fexpr: vanilla add") {
    ev(fexpr)
    assertResult(I(3)){ev("(+ 1 2)")}
  }

  test("fexpr applied constant") {
    ev(fexpr)
    assertResult(I(1)){ev("((fexpr (x) x) 1)")}
  }

  test("fexpr applied add") {
    ev(fexpr)
    assertResult(P(S("+"), P(I(1), P(I(2), N)))){ev("((fexpr (x) x) (+ 1 2))")}
  }

  test("fexpr by name applied add") {
    ev(fexpr)
    ev("(define id-fexpr (fexpr (x) x))")
    assertResult(P(S("+"), P(I(1), P(I(2), N)))){ev("(id-fexpr (+ 1 2))")}
  }

  test("fexpr apply/eval") {
    ev(fexpr)
    ev(top_eval)
    assertResult(I(1)){ev("((fexpr (x) (eval x)) 1)")}
  }

  test("fexpr apply/eval add") {
    ev(fexpr)
    ev(top_eval)
    assertResult(I(3)){ev("((fexpr (x) (eval x)) (+ 1 2))")}
  }

  test("fexpr apply/eval if") {
    ev(fexpr)
    ev(top_eval)
    assertResult(I(1)){ev("((fexpr (c a b) (if (eval c) (eval a) (eval b))) #t 1 bad)")}
  }

  test("fexpr arg") {
    ev(fexpr)
    assertResult(P(S("+"), P(I(1), P(I(2), N)))){ev("((lambda (f) (f (+ 1 2))) (fexpr (x) x))")}
  }

  test("fexpr arg compiled") {
    ev(fexpr)
    assertResult(P(S("+"), P(I(1), P(I(2), N)))){ev("((clambda (f) (f (+ 1 2))) (fexpr (x) x))")}
  }

  test("fexpr arg full compiled") {
    ev(fexpr_compiled)
    assertResult(P(S("+"), P(I(1), P(I(2), N)))){ev("((clambda (f) (f (+ 1 2))) (fexpr (x) x))")}
  }

}
