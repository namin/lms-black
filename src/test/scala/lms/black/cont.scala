package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestCont extends TestSuite with BeforeAndAfter {
  val under = "cont_"

  before {
    clean()
  }

  def resume = """(begin
(EM (EM (begin
(define pending-thunks '())

(define old-base-apply base-apply)
(set! base-apply (lambda (fa r k)
  (if (continuation? (car fa))
    (let ((v (old-base-apply fa r (lambda (x) x))))
      (set! pending-thunks (cons (lambda () (k v)) pending-thunks))
      v)
    (old-base-apply fa r k))))
)))

(define resume! (lambda ()
  (if (null? (EM (EM pending-thunks)))
    'done
    (EM (EM (let ((thunk (car pending-thunks)))
      (set! pending-thunks (cdr pending-thunks))
      (thunk)))))))

)"""

  def pushy = """(EM (EM (begin
(define old-base-apply base-apply)
(set! base-apply (lambda (fa r k)
  (if (continuation? (car fa))
    (k (old-base-apply fa r (lambda (x) x)))
    (old-base-apply fa r k))))
)))"""

  def dummy = """(EM (begin
(define old-eval-var eval-var)
(set! eval-var (lambda (e r k)
  (if (eq? 'dummy e) (begin (k 1) (k 2) (k 3))
    (old-eval-var e r k))))
))"""

  def make_ev(c: Boolean) =
    if (c) {s: String => ev(s.replace("lambda", "clambda"))} else ev(_)

  def test_resume(c: Boolean) = {
    val mev = make_ev(c)
    mev(resume)
    mev(dummy)
    assertResult(I(1)){ev("dummy")}
    assertResult(I(2)){ev("(resume!)")}
    assertResult(I(3)){ev("(resume!)")}
    assertResult(I(3)){ev("(resume!)")} // id_cont :(
    assertResult(S("done")){ev("(resume!)")}
  }

  test("resume continuations") {
    test_resume(false)
  }

  test("resume continuations (all compiled)") {
    test_resume(true)
  }

  def test_pushy(c: Boolean) = {
    val mev = make_ev(c)
    mev(pushy)
    mev(dummy)
    assertResult(I(3)){ev("dummy")}
  }

  test("pushy continuations") {
    test_pushy(false)
  }

  test("pushy continuations (all compiled)") {
    test_pushy(true)
  }
}
