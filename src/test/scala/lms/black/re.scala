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
(define matches (lambda (r)
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

  // translated from
  // http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
  def matches_bis = """(begin
(define match (lambda (r) 'TODO))
(define match_here (lambda (r) 'TODO))
(define match_star (lambda (r) 'TODO))

(define match_loop (lambda (m) (lambda (s)
  (if (m s)
      #t
      (if (null? s)
          #f
         ((match_loop m) (cdr s)))))))
(set! match (lambda (r)
  (if (null? r) (lambda (s) #t)
      (if (eq? '^ (car r))
          (match_here (cdr r))
          (match_loop (match_here r))))))

(set! match_here (lambda (r)
  (if (null? r) (lambda (s) #t)
  (let ((m (if (eq? '_ (car r))
               (lambda (s) (if (null? s) #f ((match_here (cdr r)) (cdr s))))
               (lambda (s) (if (null? s) #f
                          (if (eq? (car r) (car s))
                              ((match_here (cdr r)) (cdr s))
                              #f))))))
    (if (null? (cdr r))
        (if (eq? '$ (car r))
            (lambda (s) (null? s))
            m)
    (if (eq? '* (car (cdr r)))
        (match_star (car r) (cdr (cdr r)))
        m))))))

(define star_loop (lambda (m c) (lambda (s)
  (if (m s)
      #t
      (if (null? s)
          #f
          (if (eq? '_ c)
              ((star_loop m c) (cdr s))
              (if (eq? c (car s))
                 ((star_loop m c) (cdr s))
                 #f)))))))

(set! match_star (lambda (c r)
  (star_loop (match_here r) c)))
)
"""

  def go(c: Boolean) = {
    ev(if (c) matches_bis.replace("lambda", "clambda") else matches_bis)
    def to_list(s: String) = s.mkString("'(", " ", ")")
    def testmatch(r: String, s: String, o: Boolean) = {
      val qr = to_list(r)
      val qs = to_list(s)
      assertResult(B(o)){ev(s"((match $qr) $qs)")}
      if (c) {
        assertResult(B(o)){ev(s"(((clambda () (match $qr))) $qs)")}
      }
    }
    testmatch("^hello$", "hello", true)
    testmatch("^hello$", "hell", false)
    testmatch("hell", "hello", true);
    testmatch("hell", "hell", true);
    testmatch("hel*", "he", true);
    testmatch("hel*$", "hello", false);
    testmatch("hel*", "yo hello", true);
    testmatch("ab", "hello ab hello", true);
    testmatch("^ab", "hello ab hello", false);
    testmatch("a*b", "hello aab hello", true);
    testmatch("^ab*", "abcd", true);
    testmatch("^ab*", "a", true);
    testmatch("^ab*", "ac", true);
    testmatch("^ab*", "bac", false);
    testmatch("^ab*$", "ac", false);
  }

  test("matches bis (interpreted)") {
    go(false)
  }

  test("matches bis (compiled)") {
    go(true)
    val kv = Map(
      "start_ab_bis" -> ev("(clambda () (match '(^ a b)))").asInstanceOf[Evalfun],
      "ab_star_c" -> ev("(clambda () (match '(a b * c)))").asInstanceOf[Evalfun]
    )
    for ((k, v) <- kv) {
      checkOut(k,
        println(printer.summarize(v)),
        suffix = "txt")
    }
  }
}
