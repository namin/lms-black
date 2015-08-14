package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestRepl extends TestSuite with BeforeAndAfter {
  val under = "repl_"

  before {
    clean()
  }

  test("counter") {
    ev("(define counter 0)")
    ev("(define old-eval-var eval-var)")
    ev("""(set! eval-var (clambda (e r k)
      (if (eq? e 'n) (set! counter (+ counter 1)) 0)
      (old-eval-var e r k)))""")
    ev("(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(set! counter 0)")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(102)){ev("counter")}
    ev("(set! fib (clambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(set! counter 0)")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(7982)){ev("counter")}
    ev("(set! eval-var old-eval-var)")
    ev("(set! counter 0)")
    assertResult(I(987)){ev("(fib 16)")}
    assertResult(I(7982)){ev("counter")}
    ev("(set! fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    ev("(set! counter 0)")
    assertResult(I(13)){ev("(fib 7)")}
    assertResult(I(0)){ev("counter")}
  }
}
