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
      val x7 = {x8: (R[Value]) =>
        val x9 = x6(x8)
        x9: R[Value]
      }
      val x11 = base_apply[R](x5, List(x5), Map(("fun" -> Code[R](x1)), ("F" -> Code[R](x5))), x7)
      x11: R[Value]
    }
    val x13 = evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = x3.asInstanceOf[((Value, Cont[R])) => R[Value]]})
    val x14 = {x15: ((Value, Cont[R])) =>
      val x16 = x15._1
      val x18 = {x19: ((Value, Cont[R])) =>
        val x20 = x19._1
        val x21 = x19._2
        val x24 = {x25: (R[Value]) =>
          val x26 = x21(x25)
          x26: R[Value]
        }
        val x22 = {x23: (R[Value]) =>
          val x28 = base_apply[R](x23, List(x20), Map(("fun" -> Code[R](x1)), ("F" -> Code[R](x16)), ("x" -> Code[R](x20))), x24)
          x28: R[Value]
        }
        val x30 = base_apply[R](x16, List(x16), Map(("fun" -> Code[R](x1)), ("F" -> Code[R](x16)), ("x" -> Code[R](x20))), x22)
        x30: R[Value]
      }
      val x32 = evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = x18.asInstanceOf[((Value, Cont[R])) => R[Value]]})
      val x17 = x15._2
      val x33 = {x34: (R[Value]) =>
        val x35 = x17(x34)
        x35: R[Value]
      }
      val x37 = base_apply[R](x1, List(x32), Map(("fun" -> Code[R](x1)), ("F" -> Code[R](x16))), x33)
      x37: R[Value]
    }
    val x39 = evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = x14.asInstanceOf[((Value, Cont[R])) => R[Value]]})
    val x2 = x0._2
    val x40 = {x41: (R[Value]) =>
      val x42 = x2(x41)
      x42: R[Value]
    }
    val x44 = base_apply[R](x13, List(x39), Map(("fun" -> Code[R](x1))), x40)
    x44
  }
}
/*****************************************
End of Generated Code
*******************************************/
// compilation: ok
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
          val x24 = base_apply[R](P("+"), List(x15, x19), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x20)
          x24: R[Value]
        }
        val x16 = {x17: (R[Value]) =>
          val x26 = base_apply[R](x1, List(x17), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x18)
          x26: R[Value]
        }
        val x28 = base_apply[R](P("-"), List(x5, I(2)), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x16)
        x28: R[Value]
      }
      val x12 = {x13: (R[Value]) =>
        val x30 = base_apply[R](x1, List(x13), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x14)
        x30: R[Value]
      }
      val x7 = {x8: (R[Value]) =>
        val x9 = B(false) != x8
        val x34 = if (x9) {
          val x10 = x6(x5)
          x10
        } else {
          val x32 = base_apply[R](P("-"), List(x5, I(1)), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x12)
          x32
        }
        x34: R[Value]
      }
      val x36 = base_apply[R](P("<"), List(x5, I(2)), Map(("fib" -> Code[R](x1)), ("n" -> Code[R](x5))), x7)
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
