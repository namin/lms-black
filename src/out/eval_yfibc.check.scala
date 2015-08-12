// /*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class staged$0 extends Fun[NoRep] with (((Value, Cont[NoRep])) => Value) {
  def apply(v: (Value, Cont[NoRep])): Value = v._2(v._1)
  def fun[R[_]:Ops] = { v => fun[R]((v._1, v._2))  }
  def fun[R[_]:Ops](x0:(Value, Cont[R])): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = x0._1
    val x3 = {x4: ((Value, Cont[R])) =>
      val x7 = makePair(I(2), N)
      val x5 = x4._1
      val x8 = makePair(x5, x7)
      val x6 = x4._2
      val x30 = {x31: (R[Value]) =>
        val x32 = x6(x31)
        x32: R[Value]
      }
      val x19 = {x20: (R[Value]) =>
        val x21 = makePair(I(2), N)
        val x22 = makePair(x5, x21)
        val x26 = {x27: (R[Value]) =>
          val x28 = makePair(x27, N)
          val x29 = makePair(x20, x28)
          val x34 = base_apply[R](Prim("+"), x29, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x30)
          x34: R[Value]
        }
        val x23 = {x24: (R[Value]) =>
          val x25 = makePair(x24, N)
          val x36 = base_apply[R](x1, x25, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x26)
          x36: R[Value]
        }
        val x38 = base_apply[R](Prim("-"), x22, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x23)
        x38: R[Value]
      }
      val x16 = {x17: (R[Value]) =>
        val x18 = makePair(x17, N)
        val x40 = base_apply[R](x1, x18, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x19)
        x40: R[Value]
      }
      val x9 = {x10: (R[Value]) =>
        val x11 = B(false) != x10
        val x44 = if (x11) {
          val x12 = x6(x5)
          x12
        } else {
          val x14 = makePair(I(1), N)
          val x15 = makePair(x5, x14)
          val x42 = base_apply[R](Prim("-"), x15, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x16)
          x42
        }
        x44: R[Value]
      }
      val x46 = base_apply[R](Prim("<"), x8, P(P(S("n"), x4._1), P(P(S("fib"), x0._1), _P(118,119))), x9)
      x46: R[Value]
    }
    val x48 = evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = x3.asInstanceOf[((Value, Cont[R])) => R[Value]]})
    val x2 = x0._2
    val x49 = x2(x48)
    x49
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
