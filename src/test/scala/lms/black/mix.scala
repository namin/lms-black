package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestMix extends TestSuite with BeforeAndAfter {
  val under = "mix_"

  before {
    clean()
  }

  test("mix dynamic and static") {
    ev("(define funlst (cons (clambda (x) (+ x 1)) '()))")
    ev("(define fa1 (clambda () ((car funlst) 2)))")
    ev("(define fa2 (let ((fs funlst)) (clambda () ((car fs) 2))))")
    ev("(define fb (clambda (fs) ((car fs) 2)))")
    ev("(define fc1 (clambda () (fb funlst)))")
    ev("(define fc2 (let ((fs funlst)) (clambda () (fb fs))))")
    assertResult(I(3)){ev("(fa1)")}
    assertResult(I(3)){ev("(fa2)")}
    assertResult(I(3)){ev("(fb funlst)")}
    assertResult(I(3)){ev("(fc1)")}
    assertResult(I(3)){ev("(fc2)")}
    ev("(set! funlst (cons (clambda (x) (+ x 2)) '()))")
    assertResult(I(4)){ev("(fa1)")}
    assertResult(I(3)){ev("(fa2)")}
    assertResult(I(4)){ev("(fb funlst)")}
    assertResult(I(4)){ev("(fc1)")}
    assertResult(I(3)){ev("(fc2)")}
  }
}
