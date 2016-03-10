package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestUndo extends TestSuite with BeforeAndAfter {
  val under = "undo_"

  before {
    clean()
  }

  def reify_env = """(begin
(define old-eval-var eval-var)

(set! eval-var
  (clambda (e r k)
      (if (eq? '_env e) (k r) (old-eval-var e r k))))
)"""

  def mm_undo = """(begin
(define undo-list '())

(define old-eval-set! eval-set!)

(define list
  (lambda args args))

(set! eval-set!
  (clambda (e r k)
      (let ((name (car (cdr e))))
        (eval-var name r (lambda (v)
                           (set! undo-list (cons (cons name v) undo-list))
                           (old-eval-set! e r k))))))

(define reflect-undo!
  (clambda (r)
      (if (null? undo-list)
          '()
          (begin
            (old-eval-set!
             (list 'set! (car (car undo-list)) (list 'quote (cdr (car undo-list))))
             r
             (lambda (v) v))
            (set! undo-list (cdr undo-list))))))
)"""

  def m_undo = """
(define undo!
  (clambda ()
      ((EM reflect-undo!) _env)))
"""

  def all = s"""(begin
(EM (EM $reify_env))
(EM (EM $mm_undo))
(EM $m_undo)
)"""

  test("undo") {
    ev(all)
    ev("(EM (define old-eval-var eval-var))")
    ev("(EM (set! eval-var (clambda (e r k) (if (eq? e 'n) (k 0) (old-eval-var e r k)))))")
    ev("(define n 1)")
    assertResult(I(0)){ev("n")}
    assertResult(B(false)){ev("(EM (eq? old-eval-var eval-var))")}
    ev("(EM (undo!))")
    assertResult(I(1)){ev("n")}
    assertResult(B(true)){ev("(EM (eq? old-eval-var eval-var))")}
  }
}
