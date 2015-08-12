/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class Snippet extends Fun[NoRep] with (((Value, Cont[NoRep])) => Value) {
  def apply(v: (Value, Cont[NoRep])): Value = v._2(v._1)
  def fun[R[_]:Ops] = { v => fun[R]((v._1, v._2))  }
  def fun[R[_]:Ops](x0:(Value, Cont[R])): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = x0._1
    val x3 = {x4: ((Value, Cont[R])) =>
      val x5 = x4._1
      val x6 = x4._2
      val x20 = {x21: (R[Value]) =>
        val x22 = x6(x21)
        x22: R[Value]
      }
      val x14 = {x15: (R[Value]) =>
        val x18 = {x19: (R[Value]) =>
          val x24 = base_apply[R](P("+"), List(x15, x19), Map(("fib" -> x1), ("n" -> x5)), x20)
          x24: R[Value]
        }
        val x16 = {x17: (R[Value]) =>
          val x26 = base_apply[R](x1, List(x17), Map(("fib" -> x1), ("n" -> x5)), x18)
          x26: R[Value]
        }
        val x28 = base_apply[R](P("-"), List(x5, I(2)), Map(("fib" -> x1), ("n" -> x5)), x16)
        x28: R[Value]
      }
      val x12 = {x13: (R[Value]) =>
        val x30 = base_apply[R](x1, List(x13), Map(("fib" -> x1), ("n" -> x5)), x14)
        x30: R[Value]
      }
      val x7 = {x8: (R[Value]) =>
        val x9 = B(false) != x8
        val x34 = if (x9) {
          val x10 = x6(x5)
          x10
        } else {
          val x32 = base_apply[R](P("-"), List(x5, I(1)), Map(("fib" -> x1), ("n" -> x5)), x12)
          x32
        }
        x34: R[Value]
      }
      val x36 = base_apply[R](P("<"), List(x5, I(2)), Map(("fib" -> x1), ("n" -> x5)), x7)
      x36: R[Value]
    }
    val x38 = evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = x3.asInstanceOf[((Value, Cont[R])) => R[Value]]})
    val x2 = x0._2
    val x39 = x2(x38)
    x39
  }
}
/*****************************************
End of Generated Code
*******************************************/
// compilation: ok
