package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestBase extends TestSuite with BeforeAndAfter {
  val under = "base_"

  before {
    clean()
  }

  def compileIf(c: Boolean)(s: String) = s.replace("lambda", "clambda")

  // There's no reason to use the Y combinator, except that it
  // exercises code more subtly than a plain `define`.
  def Y(c: Boolean) = compileIf(c) {
    """(lambda (fun)
          ((lambda (F) (F F))
           (lambda (F) (fun (lambda (x) ((F F) x))))))"""
  }

  def fib(c: Boolean) = compileIf(c) {
    """(lambda (fib) (lambda (n)
          (if (< n 2)
              n
              (+ (fib (- n 1)) (fib (- n 2))))))"""
  }

  def sumf(c: Boolean) = compileIf(c) {
    """(lambda (f) (lambda (sumf) (lambda (a)
         (if (< a 0) 0
             (+ (f a) (sumf (- a 1)))))))"""
  }

  def make_counter(c: Boolean) = compileIf(c) {
    """(lambda (c)
         (lambda ()
           (set! c (+ c 1))
           (set! c (+ c 1))
           (set! c (+ c 1))
           c))"""
  }

  test("fib evaluated") {
    assertResult(I(13)){
      ev(s"((${Y(false)} ${fib(false)}) 7)")
    }
  }

  test("fib compiled") {
    assertResult(I(13)){
      ev(s"((${Y(false)} ${fib(true)}) 7)")
    }
  }

  test("Y & fib compiled") {
    assertResult(I(13)){
      ev(s"((${Y(true)} ${fib(true)}) 7)")
    }
  }

  test("sum of fib evaluated") {
    assertResult(I(33)) {
      ev(s"((${Y(false)} (${sumf(false)} (${Y(false)} ${fib(false)}))) 7)")
    }
  }

  test("sum of fib compiled") {
    assertResult(I(33)) {
      ev(s"((${Y(true)} (${sumf(true)} (${Y(true)} ${fib(true)}))) 7)")
    }
  }

  test("counter evaluated") {
    assertResult(I(3)) {
      ev(s"((${make_counter(false)} 0))")
    }
  }

  test("counter compiled") {
    assertResult(I(3)) {
      ev(s"((${make_counter(true)} 0))")
    }
  }
}
