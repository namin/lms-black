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
    val x2 = o.cellNew(x1, "c")
    val x22 = o.makeFun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1.cellRead(x2)
        val x6 = o1.makePair(x5, o1.lift(P(I(1), N)))
        val x8 = o1.app(MEnv(0), o1.lift(Prim("+")), x6, P(N, P(P(P(S("c"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x7: R1[Value]) =>
          x7: R1[Value]
        })
        val x9 = o1.cellSet(x2, x8)
        val x10 = o1.cellRead(x2)
        val x11 = o1.makePair(x10, o1.lift(P(I(1), N)))
        val x13 = o1.app(MEnv(0), o1.lift(Prim("+")), x11, P(N, P(P(P(S("c"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x12: R1[Value]) =>
          x12: R1[Value]
        })
        val x14 = o1.cellSet(x2, x13)
        val x15 = o1.cellRead(x2)
        val x16 = o1.makePair(x15, o1.lift(P(I(1), N)))
        val x18 = o1.app(MEnv(0), o1.lift(Prim("+")), x16, P(N, P(P(P(S("c"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x17: R1[Value]) =>
          x17: R1[Value]
        })
        val x19 = o1.cellSet(x2, x18)
        val x20 = o1.cellRead(x2)
        x20: R1[Value]
    }})
    x22
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
