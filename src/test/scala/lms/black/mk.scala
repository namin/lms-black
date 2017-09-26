package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter


class TestMicroKanren extends TestSuite with BeforeAndAfter {
  val under = "mk_"

  before {
    clean()
  }

  def mk = """(begin
(define list (lambda args args))
(define not (lambda (b) (if b #f #t)))

(EM (begin
(define old-eval-application eval-application)
(define and? (lambda (e) (if (pair? e) (eq? 'and (car e)) #f)))
(define apply-and (lambda (e r k)
  (base-eval (car (cdr e)) r (lambda (v1) (if v1
  (base-eval (car (cdr (cdr e))) r (lambda (v2) (k v2)))
  (k #f))))))
(set! eval-application (lambda (e r k) (if (and? e) (apply-and e r k)
  (old-eval-application e r k))))
))

(define var (lambda (c) (list 'var c)))
(define var? (lambda (x) (and (pair? x) (eq? 'var (car x)))))
(define var=? (lambda (x1 x2) (eq? (car (cdr x1)) (car (cdr x2)))))

(define assp (lambda (p s)
  (if (null? s) '()
    (if (p (car (car s))) (car s)
      (assp p (cdr s))))))

(define walk (lambda (u s)
  (let ((pr (if (var? u) (assp (lambda (v) (var=? u v)) s) '())))
    (if (not (null? pr)) (walk (cdr pr) s) u))))

(define ext-s (lambda (x v s) (cons (cons x v) s)))

(define mzero '())
(define unit (lambda (s/c) (cons s/c mzero)))

(define unify (lambda (u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (if (and (var? u) (var? v) (var=? u v)) s
    (if (var? u) (ext-s u v s)
    (if (var? v) (ext-s v u s)
    (if (and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s)))
    (if (eq? u v) s '()))))))))

(define == (lambda (u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if (not (null? s)) (unit (cons s (cdr s/c))) mzero)))))

(define call/fresh (lambda (f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1)))))))

(define mplus (lambda ($1 $2)
  (if (null? $1) $2
  (if (pair? $1) (cons (car $1) (mplus (cdr $1) $2))
  (lambda () (mplus $2 ($1)))))))
(define bind (lambda ($ g)
  (if (null? $) mzero
  (if (pair? $) (mplus (g (car $)) (bind (cdr $) g))
  (lambda () (bind ($) g))))))

(define disj (lambda (g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))))
(define conj (lambda (g1 g2) (lambda (s/c) (bind (g1 s/c) g2))))

(define empty-state (cons '() 0))

(define a-and-b
  (conj
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh
    (lambda (b)
      (disj
       (== b 5)
       (== b 6))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))
)"""

  test("and") {
    ev(mk)
    assertResult(B(false))(ev("(and #t #f)"))
  }
  test("1") {
    ev(mk)
    assertResult("((((var 0) . 5)) . 1)")(show(ev(
      "(let ((p ((call/fresh (lambda (q) (== q 5))) empty-state))) (car p))")))
  }
  test("2") {
    ev(mk)
    assertResult("()")(show(ev(
      "(let ((p ((call/fresh (lambda (q) (== q 5))) empty-state))) (cdr p))")))
  }
  test("3") {
    ev(mk)
    assertResult("((((var 1) . 5) ((var 0) . 7)) . 2)")(show(ev(
      "(let ((p (a-and-b empty-state))) (car p))")))
  }
  test("4") {
    ev(mk)
    assertResult("((((var 1) . 6) ((var 0) . 7)) . 2)")(show(ev(
      "(let ((p (a-and-b empty-state))) (car (cdr p)))")))
  }
  test("5") {
    ev(mk)
    assertResult("()")(show(ev(
      "(let ((p (a-and-b empty-state))) (cdr (cdr p)))")))
  }
  test("compiled 1") {
    ev(mk.replace("lambda", "clambda"))
    assertResult("((((var 0) . 5)) . 1)")(show(ev(
      "(let ((p ((call/fresh (clambda (q) (== q 5))) empty-state))) (car p))")))

    val p = ev("(clambda () ((call/fresh (clambda (q) (== q 5))) empty-state))").
      asInstanceOf[Evalfun]
    // not interesting, because cell reads block optimizations
    println(printer.summarize(p))
  }
}
