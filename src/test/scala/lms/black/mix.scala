package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestMix extends TestSuite with BeforeAndAfter {
  val under = "mix_"

  before {
    clean()
  }

  def go(compile: Boolean, mk_fs: String => String, f: String) = {
    val over_opt = compile && (f=="fs")
    val lambda = (if (compile) "c" else "")+"lambda"
    ev(s"(define fs ${mk_fs(s"($lambda (x) (+ x 1))")})")
    ev(s"(define t1 ($lambda () ($f 2)))")
    assertResult(I(3)){ev("(t1)")}
    ev(s"""
(define t2
  (let ((f $f))
    ($lambda () (f 2))))
""")
    assertResult(I(3)){ev("(t2)")}
    ev(s"""
(define p3
  (let ((f $f))
    (cons (lambda () (f 2))
          (lambda ()
            (let ((old_f f))
              (set! f (lambda (x) (old_f (old_f x)))))))))
""")
    ev("(define t3 (car p3))")
    ev("(define u3 (cdr p3))")
    assertResult(I(3)){ev("(t3)")}

    ev(s"(set! fs ${mk_fs(s"($lambda (x) (+ x 2))")})")
    assertResult(I(if (over_opt) 3 else 4)){ev("(t1)")}
    assertResult(I(3)){ev("(t2)")}
    assertResult(I(3)){ev("(t3)")}

    ev("(u3)")
    assertResult(I(4)){ev("(t3)")}
    ev("(u3)")
    assertResult(I(6)){ev("(t3)")}
  }

  val opt = ({f: String => f}, "fs")
  val wrap = ({f: String => s"(cons $f '())"}, "(car fs)")
  test("mix (uncompiled opt fun)") {
    go(false, opt._1, opt._2)
  }
  test("mix (uncompiled wrap fun)") {
    go(false, wrap._1, wrap._2)
  }
  test("mix (compiled opt fun)") {
    go(true, opt._1, opt._2)
  }
  test("mix (compiled wrap fun)") {
    go(true, wrap._1, wrap._2)
  }
}
