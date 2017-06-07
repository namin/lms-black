package scala.lms.black

import eval._
import repl._

import org.scalameter._
object Bench {
  def bench(thunk: => Unit) = {    
    val time = measure {
      // not sure why pink can sustain more orders of magnitude...
      for (i <- 0 until 1000) {
        thunk
      }
    }
    time
  }

  def benchFac() = {
    // Shortcut: using + instead of *, because * is unbound...
    println("fac #,evaluated,compiled,traced evaluated,traced compiled")
    for (i <- 0 to 9) {
      val e = parse(s"(fac $i)")
      clean()
      ev("(define fac (lambda (n) (if (eq? n 0) 1 (+ (fac (- n 1)) n))))")
      val t1 = bench(evl(e))
      ev("(set! fac (clambda (n) (if (eq? n 0) 1 (+ (fac (- n 1)) n))))")
      val t2 = bench(evl(e))
      clean()
      ev("(EM (define counter 0))")
      ev("(EM (define old-eval-var eval-var))")
      ev("""(EM (set! eval-var (clambda (e r k)
        (if (eq? e 'n) (set! counter (+ counter 1)) 0)
        (old-eval-var e r k))))""")
      ev("(define fac (lambda (n) (if (eq? n 0) 1 (+ (fac (- n 1)) n))))")
      val t3 = bench(evl(e))
      ev("(set! fac (clambda (n) (if (eq? n 0) 1 (+ (fac (- n 1)) n))))")
      val t4 = bench(evl(e))
      println(s"$i,$t1,$t2,$t3,$t4")
    }
  }

  def main(args: Array[String]) {
    verbose = false
    benchFac()
  }
}
