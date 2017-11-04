package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestDelta extends TestSuite with BeforeAndAfter {
  val under = "delta_"

  before {
    clean()
  }

  def reify_env = """(begin
(define old-eval-var eval-var)

(set! eval-var
  (clambda (e r k)
      (if (eq? '_env e) (k r) (old-eval-var e r k))))
)"""

  def list = """
(define list
  (lambda args args))
"""

  def delta = s"""(begin
$list

(define old-eval-application eval-application)

(define delta?
  (lambda (e)
    (if (pair? e) (if (pair? (car e)) (eq? 'delta (car (car e))) #f) #f)))

(define id-cont
  (lambda (v) v))

(define make-pairs
  (lambda (ks vs)
      (if (null? ks) '()
          (if (null? vs) '()
              (if (symbol? ks) (cons (cons ks vs) '())
                  (cons (cons (car ks) (car vs)) (make-pairs (cdr ks) (cdr vs))))))))

(define extend
  (lambda (env params args)
      (cons (make-pairs params args) env)))

(define lookup
  (lambda (r e) (eval-var e r (lambda (x) x))))

(define apply-delta
  (lambda (e r k)
    (let ((operator (car e))
          (operand (cdr e)))
      (let ((delta-params (car (cdr operator)))
            (delta-body (cdr (cdr operator))))
        ((EM eval-begin)
         delta-body
         (extend _env delta-params (list operand r k))
         id-cont)))))

(set! eval-application
      (lambda (e r k)
        (if (delta? e)
            (apply-delta e r k)
            (old-eval-application e r k))))

(define meaning base-eval)
)"""

  def call_cc = """(begin
(define call/cc
  (lambda (f)
      ((delta (e r k)
              (k ((meaning 'f r (lambda (v) v)) k))))))
)"""

  def em_delta = s"""(begin
(EM (EM $reify_env))
(EM $delta)
)"""

  def all = s"""(begin
$em_delta
$call_cc
)"""

  def cev(s: String) = ev(s.replace("(lambda", "(clambda"))

  def go(c: Boolean) = {
    def assertEv(v: Value)(s: String) = {
      assertResult(v){ev(s)}
      assertResult(v){cev(s)}
    }
    if (c) cev(all) else ev(all)
    assertEv(I(1)){"(+ 1 (call/cc (lambda (k) 0)))"}
    assertEv(I(1)){"(+ 1 (call/cc (lambda (k) (k 0))))"}
    assertEv(I(2)){"(+ 1 (call/cc (lambda (k) (begin (k 1) (k 3)))))"}
    assertEv(I(2)){"(+ 1 (call/cc (lambda (k) (begin (k (k 1)) (k 3)))))"}
  }

  test("delta") {
    go(false)
  }

  test("delta (all-compiled)") {
    go(true)
  }


  // inspired by Blond permute! but more mundane...
  // just a hard-coding an observation of a local tower,
  // where meta and meta-meta are swapped
  // note that EM would still just rigidly follow the original meta-environment chains
  // of course, eval-EM could be redefined...
  def permute = s"""
(define permute (lambda ()
  ((delta (e0 r0 k0)
    ((delta (e1 r1 k1)
      ((delta (e2 r2 k2)
        (let ((R2 (extend r2 '(R0 K0) (list
                    (lookup (lookup r2 'r1) 'r0)
                    (lookup (lookup r2 'r1) 'k0))))
              (K2 k2))
        (let ((R1 (extend (lookup r2 'r1) '(R2 K2) (list R2 K2)))
              (K1 (lookup r2 'k1)))
        (meaning '(cons (meaning '(cons (meaning 'level R0 K0) level) R2 K2) level) R1 K1)
))))))))))
"""

  def all_permute = s"""(begin
$list
$em_delta
(EM $em_delta)
(EM (EM $em_delta))
$permute
(define level 'user)
(EM (define level 'meta))
(EM (EM (define level 'meta-meta)))
(permute)
)"""

  test("permute") {
    val result = P(P(S("user"),S("meta-meta")),S("meta"))
    assertResult{result}{ev(all_permute)}
    // assertResult{result}{cev(all_permute)} // TODO: generated code doesn't compile due to unbound variable...
  }
}
