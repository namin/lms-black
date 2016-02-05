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
    val x39 = o.makeFun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1.getCar(x4)
        val x6 = x5
        val x37 = o1.makeFun(new Fun[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x8: R1[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2)
            implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x)
            val x9 = o2.getCar(x8)
            val x10 = x9
            val x12 = x9
            val x13 = o2.makePair(x12, o2.lift(P(I(0), N)))
            val x15 = o2.app(MEnv(0), o2.lift(Prim("<")), x13, P(P(P(S("a"), Code(x10)), N), P(P(P(S("sumf"), Code(x6)), N), P(P(P(S("f"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x14: R2[Value]) =>
              x14: R2[Value]
            })
            val x16 = o2.isTrue(x15)
            val x35 = o2.ifThenElse((x16), {
              o2.lift(I(0))
            }, {
              val x17 = x1
              val x18 = x9
              val x19 = o2.makePair(x18, o2.lift(N))
              val x21 = o2.app(MEnv(0), x17, x19, P(P(P(S("a"), Code(x10)), N), P(P(P(S("sumf"), Code(x6)), N), P(P(P(S("f"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x20: R2[Value]) =>
                x20: R2[Value]
              })
              val x22 = x5
              val x23 = x9
              val x24 = o2.makePair(x23, o2.lift(P(I(1), N)))
              val x26 = o2.app(MEnv(0), o2.lift(Prim("-")), x24, P(P(P(S("a"), Code(x10)), N), P(P(P(S("sumf"), Code(x6)), N), P(P(P(S("f"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x25: R2[Value]) =>
                x25: R2[Value]
              })
              val x27 = o2.makePair(x26, o2.lift(N))
              val x29 = o2.app(MEnv(0), x22, x27, P(P(P(S("a"), Code(x10)), N), P(P(P(S("sumf"), Code(x6)), N), P(P(P(S("f"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x28: R2[Value]) =>
                x28: R2[Value]
              })
              val x30 = o2.makePair(x29, o2.lift(N))
              val x31 = o2.makePair(x21, x30)
              val x33 = o2.app(MEnv(0), o2.lift(Prim("+")), x31, P(P(P(S("a"), Code(x10)), N), P(P(P(S("sumf"), Code(x6)), N), P(P(P(S("f"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x32: R2[Value]) =>
                x32: R2[Value]
              })
              x33
            })(o2.valueTag)
            x35: R2[Value]
        }})
        x37: R1[Value]
    }})
    x39
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
