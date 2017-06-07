package scala.lms.black

import eval._
import repl._

import org.scalameter._
object Bench {
  def bench(thunk: => Unit) = {    
    val time = measure {
      // not sure why pink can sustain two more orders of magnitude...
      for (i <- 0 until 1000) {
        thunk
      }
    }
    time
  }

  def benchFac() = {
    // Shortcut: using + instead of *, because * is unbound...
    println("fac #,evaluated,compiled,traced evaluated,traced compiled")
    for (i <- 1 to 10) {
      clean()
      ev("(define fac (lambda (n) (if (< n 2) 1 (+ (fac (- n 1)) n))))")
      val t1 = bench(ev(s"(fac $i)"))
      ev("(set! fac (clambda (n) (if (< n 2) 1 (+ (fac (- n 1)) n))))")
      val t2 = bench(ev(s"(fac $i)"))
      ev("(EM (define counter 0))")
      ev("(EM (define old-eval-var eval-var))")
      ev("""(EM (set! eval-var (clambda (e r k)
        (if (eq? e 'n) (set! counter (+ counter 1)) 0)
        (old-eval-var e r k))))""")
      ev("(set! fac (lambda (n) (if (< n 2) 1 (+ (fac (- n 1)) n))))")
      val t3 = bench(ev(s"(fac $i)"))
      ev("(set! fac (clambda (n) (if (< n 2) 1 (+ (fac (- n 1)) n))))")
      val t4 = bench(ev(s"(fac $i)"))
      println(s"$i,$t1,$t2,$t3,$t4")
    }
  }

  def main(args: Array[String]) {
    verbose = false
    benchFac()
  }
}
