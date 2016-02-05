/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class staged$0 extends Fun[NoRep] with (Value => Value) {
  def apply(v: Value): Value = v
  def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { v => fun[R](v)(implicitly[Ops[R]], ev)  }
  def fun[R[_]:Ops](x0:Value)(implicit ev: Convert[NoRep,R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = o.getCar(x0)
    val x2 = x1
    val x38 = o.makeFun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1.getCar(x4)
        val x6 = x5
        val x8 = x5
        val x9 = o1.makePair(x8, o1.lift(P(I(2), N)))
        val x11 = o1.app(MEnv(0), o1.lift(Prim("<")), x9, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x10: R1[Value]) =>
          x10: R1[Value]
        })
        val x12 = o1.isTrue(x11)
        val x36 = o1.ifThenElse((x12), {
          val x13 = x5
          x13
        }, {
          val x15 = x1
          val x16 = x5
          val x17 = o1.makePair(x16, o1.lift(P(I(1), N)))
          val x19 = o1.app(MEnv(0), o1.lift(Prim("-")), x17, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x18: R1[Value]) =>
            x18: R1[Value]
          })
          val x20 = o1.makePair(x19, o1.lift(N))
          val x22 = o1.app(MEnv(0), x15, x20, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x21: R1[Value]) =>
            x21: R1[Value]
          })
          val x23 = x1
          val x24 = x5
          val x25 = o1.makePair(x24, o1.lift(P(I(2), N)))
          val x27 = o1.app(MEnv(0), o1.lift(Prim("-")), x25, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x26: R1[Value]) =>
            x26: R1[Value]
          })
          val x28 = o1.makePair(x27, o1.lift(N))
          val x30 = o1.app(MEnv(0), x23, x28, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x29: R1[Value]) =>
            x29: R1[Value]
          })
          val x31 = o1.makePair(x30, o1.lift(N))
          val x32 = o1.makePair(x22, x31)
          val x34 = o1.app(MEnv(0), o1.lift(Prim("+")), x32, P(P(P(S("n"), Code(x6)), N), P(P(P(S("fib"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x33: R1[Value]) =>
            x33: R1[Value]
          })
          x34
        })(o1.valueTag)
        x36: R1[Value]
    }})
    x38
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
