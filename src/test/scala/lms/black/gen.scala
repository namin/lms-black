package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestGen extends TestSuite with BeforeAndAfter {
  val under = ""

  before {
    clean()
  }

  def genfib(fn: String) = {
    ev("(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
    checkOut(fn,
      ev("(set! fib (clambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))"))
    checkOut(fn,
      println(printer.summarize(ev("fib").asInstanceOf[Evalfun])),
      suffix = "txt")
  }

  test("code generation for fib") {
    genfib("fib")
  }

  test("code generation for fib under var counter") {
    ev("(EM (define counter 0))")
    ev("(EM (define old-eval-var eval-var))")
    ev("""(EM (set! eval-var (clambda (e r k)
      (if (eq? e 'n) (set! counter (+ counter 1)) 0)
      (old-eval-var e r k))))""")
    genfib("fib_em_var_counter")
  }
}
