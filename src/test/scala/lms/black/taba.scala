package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

// There and Back Again
// http://www.brics.dk/RS/01/39/BRICS-RS-01-39.pdf
// http://www.brics.dk/RS/05/3/BRICS-RS-05-3.pdf

class TestTaba extends TestSuite with BeforeAndAfter {
  val under = "taba_"

  before {
    clean()
  }

  def add_app_hook = """
(define add-app-hook!
  (lambda (n ev)
    (let ((original-eval-application eval-application))
      (set! eval-application
            (lambda (exp env cont)
              (if (eq? (car exp) n)
                  (ev exp env cont)
                  (original-eval-application exp env cont)))))))
"""

  def taba = """(begin
(define eval-taba-call
  (lambda (add! original-eval-application)
    (lambda (exp env cont)
      (eval-list
       (cdr exp) env
       (lambda (ans-args)
         (original-eval-application
          exp env
          (lambda (ans)
            (add! ans-args ans)
            (cont ans))))))))

(define map
  (lambda (f xs)
    (if (null? xs)
        '()
        (cons (f (car xs)) (map f (cdr xs))))))

(define list (lambda args args))

(define eval-taba
  (lambda (fns)
    (lambda (exp env cont)
      (let ((original-eval-application eval-application)
            (stack '()))
        (map (lambda (fn)
               (add-app-hook!
                fn
                (eval-taba-call
                 (lambda (ans-args ans)
                   (set! stack (cons (list fn ans-args ans) stack)))
                 eval-application)))
             fns)
        (base-eval
         exp env
         (lambda (ans)
           (set! eval-application original-eval-application)
           (cont
            (list ans stack))))))))

(add-app-hook!
 'taba
 (lambda (exp env cont)
   ((eval-taba (car (cdr exp))) (car (cdr (cdr exp))) env cont)))
)"""

  def cnv = """(begin
(define zip
  (lambda (xs ys)
    (if (if (null? xs) #t (null? ys))
       '()
       (cons
        (cons (car xs) (car ys))
        (zip (cdr xs) (cdr ys))))))

(define walk
  (lambda (xs ys)
     (if (null? xs)
         (cons '() ys)
         (let ((rys (walk (cdr xs) ys)))
           (let ((r (car rys))
                 (ys (cdr rys)))
             (cons (cons (cons (car xs) (car ys)) r)
                   (cdr ys)))))))
(define cnv
  (lambda (xs ys)
    (car (walk xs ys))))
)"""

  def cnv_ex = "(taba (cnv walk) (cnv '(1 2 3) '(a b c)))"
  def cnv_ex_res = """(((1 . c) (2 . b) (3 . a))
((cnv ((1 2 3) (a b c)) ((1 . c) (2 . b) (3 . a)))
(walk ((1 2 3) (a b c)) (((1 . c) (2 . b) (3 . a))))
(walk ((2 3) (a b c)) (((2 . b) (3 . a)) c))
(walk ((3) (a b c)) (((3 . a)) b c))
(walk (() (a b c)) (() a b c))))""".replace("\n", " ")

  test("TABA cnv") {
    ev(s"(EM $add_app_hook)")
    ev(s"(EM $taba)")
    ev(cnv)
    assertResult{cnv_ex_res}{show(ev(cnv_ex))}
  }

  test("TABA cnv (meta-compiled)") {
    ev(s"(EM $add_app_hook)".replace("lambda", "clambda"))
    ev(s"(EM $taba)".replace("lambda", "clambda"))
    ev(cnv)
    assertResult{cnv_ex_res}{show(ev(cnv_ex))}
  }

  test("TABA cnv (all compiled)") {
    ev(s"(EM $add_app_hook)".replace("lambda", "clambda"))
    ev(s"(EM $taba)".replace("lambda", "clambda"))
    assertResult{cnv_ex_res}{
      show(ev(s"(taba (cnv walk) (let () ${cnv.replace("lambda", "clambda")} (cnv '(1 2 3) '(a b c))))"))
    }
  }

  // translated from
  // section 2.3 A direct-style solution
  // of http://www.brics.dk/RS/01/39/BRICS-RS-01-39.pdf
  def fields = """(begin
(define fields 'TODO)
(define word 'TODO)
(set! fields (lambda (xs)
  (if (null? xs) '()
  (if (eq? 0 (car xs)) (fields (cdr xs))
  (let ((ws (word (cdr xs))))
    (cons (cons (car xs) (car ws)) (fields (cdr ws))))))))
(set! word (lambda (xs)
  (if (null? xs) (cons '() '())
  (if (eq? 0 (car xs)) (cons '() (cdr xs))
  (let ((ws (word (cdr xs))))
    (cons (cons (car xs) (car ws)) (cdr ws)))))))
)"""

  def fields_ex = "(taba (fields word) (fields '(0 1 2 0 3 4 5 0 6)))"

  // NOTE: This might be clearer if we showed the trace without grouping per function name.
  //       Still the trace is helpful to understand what each function is responsible for.
  def fields_ex_res = """(((1 2) (3 4 5) (6))
((fields ((0 1 2 0 3 4 5 0 6)) ((1 2) (3 4 5) (6)))
(fields ((1 2 0 3 4 5 0 6)) ((1 2) (3 4 5) (6)))
(fields ((3 4 5 0 6)) ((3 4 5) (6)))
(fields ((6)) ((6)))
(fields (()) ())
(word (()) (()))
(word ((4 5 0 6)) ((4 5) 6))
(word ((5 0 6)) ((5) 6))
(word ((0 6)) (() 6))
(word ((2 0 3 4 5 0 6)) ((2) 3 4 5 0 6))
(word ((0 3 4 5 0 6)) (() 3 4 5 0 6))))""".replace("\n", " ")

  test("TABA fields") {
    ev(s"(EM $add_app_hook)")
    ev(s"(EM $taba)")
    ev(fields)
    assertResult{fields_ex_res}{show(ev(fields_ex))}
  }

  test("TABA fields (meta-compiled)") {
    ev(s"(EM $add_app_hook)".replace("lambda", "clambda"))
    ev(s"(EM $taba)".replace("lambda", "clambda"))
    ev(fields)
    assertResult{fields_ex_res}{show(ev(fields_ex))}
  }

  test("TABA fields (all compiled)") {
    ev(s"(EM $add_app_hook)".replace("lambda", "clambda"))
    ev(s"(EM $taba)".replace("lambda", "clambda"))
    assertResult{fields_ex_res}{
      show(ev(s"(taba (fields word) (let () ${fields.replace("lambda", "clambda")} (fields '(0 1 2 0 3 4 5 0 6))))"))
    }
  }

}
