package scala.lms.black

import eval._
import repl._

import org.scalatest.BeforeAndAfter

class TestInstr extends TestSuite with BeforeAndAfter {
  val under = "instr_"

  before {
    clean()
  }

  def eval_instr = """(begin
(define map
  (lambda (f xs)
    (if (null? xs)
        '()
        (cons (f (car xs)) (map f (cdr xs))))))

(define eval-instr
  (lambda (exp env cont)
    (let ((restore-thunks '())
          (total-counter 0)
          (display-msg (lambda (msg counter)
                         (display "#") (display msg) (display ": ")
                         (display counter) (newline))))
      (let ((add-instr!
             (lambda (msg original set-original!)
               (let ((counter 0))
                 (set! restore-thunks
                       (cons (lambda ()
                               (set! total-counter (+ total-counter counter))
                               (set-original! original)
                               (display-msg msg counter))
                             restore-thunks))
                 (set-original! (lambda (exp env cont)
                                  (set! counter (+ counter 1))
                                  (original exp env cont)))))))
        (add-instr! 'app eval-application (lambda (v) (set! eval-application v)))
        (add-instr! 'lam eval-lambda (lambda (v) (set! eval-lambda v)))
        (add-instr! 'var eval-var (lambda (v) (set! eval-var v))))
      (base-eval exp env (lambda (ans)
                           (newline)
                           (map  (lambda (t) (t)) restore-thunks)
                           (display-msg 'total total-counter)
                           (cont ans))))))
)"""
  def hook_instr = """
(let ((original-eval-application eval-application))
  (set! eval-application
	(lambda (exp env cont)
	    (if (eq? (car exp) 'instr)
		  (eval-instr (car (cdr exp)) env cont)
		  (original-eval-application exp env cont)))))
"""

  def church = """(begin
(define tru
  (lambda (t) (lambda (f) t)))

(define fls
  (lambda (t) (lambda (f) f)))

(define to_bool
  (lambda (b) (b #t #f)))

(define pair
  (lambda (f) (lambda (s) (lambda (b) ((b f) s)))))

(define fst
  (lambda (p) (p tru)))

(define snd
  (lambda (p) (p fls)))

(define to_tuple
  (lambda (p) (cons (fst p) (snd p))))

(define c0
  (lambda (s) (lambda (z) z)))

(define c1
  (lambda (s) (lambda (z) (s z))))

(define c2
  (lambda (s) (lambda (z) (s (s z)))))

(define scc
  (lambda (n) (lambda (s)  (lambda (z)  (s ((n s) z))))))

(define to-int
  (lambda (n) ((n (lambda (v) (+ v 1))) 0)))

(define iszro
  (lambda (n) ((n (lambda (x) fls)) tru)))

(define plus
  (lambda (m) (lambda (n) (lambda (s) (lambda (z) ((m s) ((n s) z)))))))

(define zz
  ((pair c0) c0))

(define ss
  (lambda (p) ((pair (snd p)) ((plus c1) (snd p)))))

(define prd
  (lambda (n) (fst ((n ss) zz))))

(define prd-alt
  (lambda (n) (lambda (s) (lambda (z) (((n (lambda (g) (lambda (h) (h (g s))))) (lambda (u) z)) (lambda (u) u))))))
)"""

  test("instrument church numerals") {
    ev(s"(EM $eval_instr)")
    ev(s"(EM $hook_instr)")
    ev(church)
    assertResult{"""
#var: 65
#lam: 18
#app: 42
#total: 125
"""}{captureOut{ev("(instr (to-int (prd c2)))")}}
    assertResult{"""
#var: 18
#lam: 9
#app: 14
#total: 41
"""}{captureOut{ev("(instr (to-int (prd-alt c2)))")}}
  }
}
