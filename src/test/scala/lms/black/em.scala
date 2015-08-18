package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestEM extends TestSuite with BeforeAndAfter {
  val under = "em_"

  before {
    clean()
  }

  test("var counter") {
    ev("(EM (define counter 0))")
    ev("(EM (define old-eval-var eval-var))")
    ev("""(EM (set! eval-var (clambda (e r k)
      (if (eq? e 'n) (set! counter (+ counter 1)) 0)
      (old-eval-var e r k))))""")
    ev("(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(102)){ev("(EM counter)")}
    ev("(set! fib (clambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(7982)){ev("(EM counter)")}
    ev("(EM (set! eval-var old-eval-var))")
    ev("(EM (set! counter 0))")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(7982)){ev("(EM counter)")}
    ev("(set! fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(0)){ev("(EM counter)")}
  }

  test("app counter") {
    ev("(EM (define counter 0))")
    ev("(EM (define old-eval-application eval-application))")
    ev("""(EM (set! eval-application (clambda (e r k)
      (set! counter (+ counter 1))
      (old-eval-application e r k))))""")
    ev("(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(142)){ev("(EM counter)")}
    ev("(set! fib (clambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(11174)){ev("(EM counter)")}
    ev("(EM (set! eval-application old-eval-application))")
    ev("(EM (set! counter 0))")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(11173)){ev("(EM counter)")}
    ev("(set! fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(EM (set! counter 0))")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(0)){ev("(EM counter)")}
  }

  test("user continuations") {
    ev("(EM (define old-eval-var eval-var))")
    ev("(EM (set! eval-var (clambda (e r k) (old-eval-var e r (clambda (a) (if (number? a) (k (+ a 1)) (k a)))))))")
    ev("(define id (lambda (n) (if (< n 0) (id 0) n)))")
    assertResult(I(1)){ev("(id 0)")}
    ev("(set! id (clambda (n) (if (< n 0) (id 0) n)))")
    assertResult(I(1)){ev("(id 0)")}
  }

  test("code generation for fib under var counter") {
    ev("(EM (define counter 0))")
    ev("(EM (define old-eval-var eval-var))")
    ev("""(EM (set! eval-var (clambda (e r k)
      (if (eq? e 'n) (set! counter (+ counter 1)) 0)
      (old-eval-var e r k))))""")
    ev("(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    checkOut("fib_var_counter",
      ev("(set! fib (clambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))"))
  }
}
