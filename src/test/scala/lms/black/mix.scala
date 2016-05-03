package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestMix extends TestSuite with BeforeAndAfter {
  val under = "mix_"

  before {
    clean()
  }

  def go(compile: Boolean, mk: String => String, sl: String => String) = {
    val f = sl("fs")
    val over_opt = compile && (f=="fs")
    val lambda = (if (compile) "c" else "")+"lambda"
    ev(s"(define fs ${mk(s"($lambda (x) (+ x 1))")})")
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
    (cons ($lambda () (f 2))
          ($lambda ()
            (let ((old_f f))
              (set! f ($lambda (x) (old_f (old_f x)))))))))
""")
    ev("(define t3 (car p3))")
    ev("(define u3 (cdr p3))")
    assertResult(I(3)){ev("(t3)")}

    ev(s"(set! fs ${mk(s"($lambda (x) (+ x 2))")})")
    assertResult(I(if (over_opt) 3 else 4)){ev("(t1)")}
    assertResult(I(3)){ev("(t2)")}
    assertResult(I(3)){ev("(t3)")}

    ev("(u3)")
    assertResult(I(if (compile) 3 else 4)){ev("(t3)")}
    ev("(u3)")
    assertResult(I(if (compile) 3 else 6)){ev("(t3)")}

    // Currently, we over-optimize cell reads only in
    // _direct_ function application position.
    // Hence,  (f 2) does get optimized, while ((car fs) 2) does not.
    val t1_nopt = """{(k, xs) =>
_app(`car, _cons(_cell_read(<cell fs>), `()), _cont{v_1 =>
_app(v_1, `(2), _cont{v_2 =>
_app(k, _cons(v_2, `()), _cont{v_3 =>
v_3})})})}"""
    val t_opt = "{(k, xs) =>_app(k, `(3), _cont{v_1 =>v_1})}"
    if (compile) {
      if (over_opt) assertResult(t_opt){printer.summarize(ev("t1")).replace("\n", "")}
      else assertResult(t1_nopt){printer.summarize(ev("t1"))}
      assertResult(t_opt){printer.summarize(ev("t2")).replace("\n", "")}
      assertResult(t_opt){printer.summarize(ev("t3")).replace("\n", "")}
    }
  }

  val opt = ({f: String => f}, {fs: String => fs})
  val wrap = ({f: String => s"(cons $f '())"}, {fs: String => s"(car $fs)"})
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

  test("let and cst") {
    val or = "{(k, xs) =>_app(k, `(3), _cont{v_1 =>v_1})}"
    def pev(e: String) = printer.summarize(ev(e)).replace("\n", "")
    val o1 = "(let ((f (clambda (x) (+ x 1)))) (clambda () (f 2)))"
    val n2 = "(let ((fa (cons (clambda (x) (+ x 1)) 2))) (clambda () ((car fa) (cdr fa))))"
    val o2 = "(cst ((fa (cons (clambda (x) (+ x 1)) 2))) (clambda () ((car fa) (cdr fa))))"
    for (o <- List(o1, o2, n2)) assertResult(I(3)){ev(s"($o)")}
    for (o <- List(o1, o2)) assertResult(or){pev(o)}
  }
}
