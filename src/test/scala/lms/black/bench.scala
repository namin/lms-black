package scala.lms.black

import eval._
import repl._

import org.scalameter._
object Bench {
  def bench(thunk: => Unit, n: Int = 100000) = {
    val time = measure {
      for (i <- 0 until n) {
        thunk
      }
    }
    if (n < 100000) {
      val r = 100000/n
      Quantity(r*time.value, time.units)
    } else {
      time
    }
  }

  def benchFac() = {
    println("fac #,evaluated,compiled,traced evaluated,traced compiled")
    for (i <- 0 to 9) {
      val e = parse(s"(fac $i)")
      clean()
      ev("(define fac (lambda (n) (if (eq? n 0) 1 (* n (fac (- n 1))))))")
      val t1 = bench(evl(e), n=1000)
      ev("(set! fac (clambda (n) (if (eq? n 0) 1 (* n (fac (- n 1))))))")
      val t2 = bench(evl(e))
      clean()
      ev("(EM (define counter 0))")
      ev("(EM (define old-eval-var eval-var))")
      ev("""(EM (set! eval-var (clambda (e r k)
        (if (eq? e 'n) (set! counter (+ counter 1)) 0)
        (old-eval-var e r k))))""")
      ev("(define fac (lambda (n) (if (eq? n 0) 1 (* n (fac (- n 1))))))")
      val t3 = bench(evl(e), n=1000)
      ev("(set! fac (clambda (n) (if (eq? n 0) 1 (* n (fac (- n 1))))))")
      val t4 = bench(evl(e))
      println(s"$i,$t1,$t2,$t3,$t4")
    }
  }

  def main(args: Array[String]) {
    verbose = false
    benchFac()
  }
}
