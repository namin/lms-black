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
    val x14 = o.makeFun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1.getCar(x4)
        val x6 = x5
        val x8 = x5
        val x9 = x5
        val x10 = o1.makePair(x9, o1.lift(N))
        val x12 = o1.app(MEnv(0), x8, x10, P(P(P(S("F"), Code(x6)), N), P(P(P(S("fun"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x11: R1[Value]) =>
          x11: R1[Value]
        })
        x12: R1[Value]
    }})
    val x40 = o.makeFun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x15: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x16 = o1.getCar(x15)
        val x17 = x16
        val x19 = x1
        val x35 = o1.makeFun(new Fun[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x20: R1[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2)
            implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x)
            val x21 = o2.getCar(x20)
            val x22 = x21
            val x24 = x16
            val x25 = x16
            val x26 = o2.makePair(x25, o2.lift(N))
            val x33 = o2.app(MEnv(0), x24, x26, P(P(P(S("x"), Code(x22)), N), P(P(P(S("F"), Code(x17)), N), P(P(P(S("fun"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x27: R2[Value]) =>
              val x28 = x21
              val x29 = o2.makePair(x28, o2.lift(N))
              val x31 = o2.app(MEnv(0), x27, x29, P(P(P(S("x"), Code(x22)), N), P(P(P(S("F"), Code(x17)), N), P(P(P(S("fun"), Code(x2)), N), P(Cell("global"), N)))), mkCont[R2]{(x30: R2[Value]) =>
                x30: R2[Value]
              })
              x31: R2[Value]
            })
            x33: R2[Value]
        }})
        val x36 = o1.makePair(x35, o1.lift(N))
        val x38 = o1.app(MEnv(0), x19, x36, P(P(P(S("F"), Code(x17)), N), P(P(P(S("fun"), Code(x2)), N), P(Cell("global"), N))), mkCont[R1]{(x37: R1[Value]) =>
          x37: R1[Value]
        })
        x38: R1[Value]
    }})
    val x41 = o.makePair(x40, o.lift(N))
    val x43 = o.app(MEnv(0), x14, x41, P(P(P(S("fun"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x42: R[Value]) =>
      x42: R[Value]
    })
    x43
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
