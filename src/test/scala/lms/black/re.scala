package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestRe extends TestSuite with BeforeAndAfter {
  val under = "re_"

  before {
    clean()
  }

  def list = """
(define list (clambda args args))
"""

  def matches = """(begin
(define matches (lambda (r) 'TODO))
(set! matches (lambda (r)
  (if (null? r) (lambda (s) #t)
    (lambda (s) (if (null? s) #f
           (if (eq? (car r) (car s)) ((matches (cdr r)) (cdr s)) #f))))))
)
"""

  test("matches (interpreted)") {
    ev(list)
    ev(matches)
    assertResult(B(false)){ev("((matches '(a b)) '(a c))")}
    assertResult(B(true)){ev("((matches '(a b)) '(a b))")}
    assertResult(B(true)){ev("((matches '(a b)) '(a b c))")}
  }

  test("matches (compiled)") {
    ev(list)
    ev(matches.replace("lambda", "clambda"))
    assertResult(B(false)){ev("((matches '(a b)) '(a c))")}
    assertResult(B(true)){ev("((matches '(a b)) '(a b))")}
    assertResult(B(true)){ev("((matches '(a b)) '(a b c))")}
    val f1 = ev("(clambda () (matches '(a b)))").asInstanceOf[Evalfun]
    val f2 = ev("(clambda () (matches (cons 'a (cons 'b '()))))").asInstanceOf[Evalfun]
    val f3 = ev("(clambda () (matches (list 'a 'b)))").asInstanceOf[Evalfun]
    for (f <- List(f1, f2, f3)) {
      checkOut("start_ab",
        println(printer.summarize(f)),
        suffix = "txt")
    }
  }
}
