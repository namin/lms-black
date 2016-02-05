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
    val x5 = o.cellRead(o.lift(Cell("counter")))
    val x6 = o.makePair(x5, o.lift(P(I(1), N)))
    val x8 = o.app(MEnv(1), o.lift(Prim("+")), x6, P(P(P(S("e"), Code(S("n"))), P(P(S("r"), Code(P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)))), P(P(S("k"), Code(Cont(120))), N))), P(Cell("global1"), N)), mkCont[R]{(x7: R[Value]) =>
      x7: R[Value]
    })
    val x9 = o.cellSet(o.lift(Cell("counter")), x8)
    val x11 = x1
    val x12 = o.makePair(x11, o.lift(P(I(2), N)))
    val x14 = o.app(MEnv(0), o.lift(Prim("<")), x12, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x13: R[Value]) =>
      x13: R[Value]
    })
    val x15 = o.isTrue(x14)
    val x62 = o.ifThenElse((x15), {
      val x16 = o.cellRead(o.lift(Cell("counter")))
      val x17 = o.makePair(x16, o.lift(P(I(1), N)))
      val x19 = o.app(MEnv(1), o.lift(Prim("+")), x17, P(P(P(S("e"), Code(S("n"))), P(P(S("r"), Code(P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)))), P(P(S("k"), Code(Cont(131))), N))), P(Cell("global1"), N)), mkCont[R]{(x18: R[Value]) =>
        x18: R[Value]
      })
      val x20 = o.cellSet(o.lift(Cell("counter")), x19)
      val x22 = x1
      x22
    }, {
      val x26 = o.cellRead(o.lift(Cell("fib")))
      val x28 = o.cellRead(o.lift(Cell("counter")))
      val x29 = o.makePair(x28, o.lift(P(I(1), N)))
      val x31 = o.app(MEnv(1), o.lift(Prim("+")), x29, P(P(P(S("e"), Code(S("n"))), P(P(S("r"), Code(P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)))), P(P(S("k"), Code(Cont(166))), N))), P(Cell("global1"), N)), mkCont[R]{(x30: R[Value]) =>
        x30: R[Value]
      })
      val x32 = o.cellSet(o.lift(Cell("counter")), x31)
      val x34 = x1
      val x35 = o.makePair(x34, o.lift(P(I(1), N)))
      val x37 = o.app(MEnv(0), o.lift(Prim("-")), x35, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x36: R[Value]) =>
        x36: R[Value]
      })
      val x38 = o.makePair(x37, o.lift(N))
      val x40 = o.app(MEnv(0), x26, x38, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x39: R[Value]) =>
        x39: R[Value]
      })
      val x42 = o.cellRead(o.lift(Cell("fib")))
      val x44 = o.cellRead(o.lift(Cell("counter")))
      val x45 = o.makePair(x44, o.lift(P(I(1), N)))
      val x47 = o.app(MEnv(1), o.lift(Prim("+")), x45, P(P(P(S("e"), Code(S("n"))), P(P(S("r"), Code(P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)))), P(P(S("k"), Code(Cont(202))), N))), P(Cell("global1"), N)), mkCont[R]{(x46: R[Value]) =>
        x46: R[Value]
      })
      val x48 = o.cellSet(o.lift(Cell("counter")), x47)
      val x50 = x1
      val x51 = o.makePair(x50, o.lift(P(I(2), N)))
      val x53 = o.app(MEnv(0), o.lift(Prim("-")), x51, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x52: R[Value]) =>
        x52: R[Value]
      })
      val x54 = o.makePair(x53, o.lift(N))
      val x56 = o.app(MEnv(0), x42, x54, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x55: R[Value]) =>
        x55: R[Value]
      })
      val x57 = o.makePair(x56, o.lift(N))
      val x58 = o.makePair(x40, x57)
      val x60 = o.app(MEnv(0), o.lift(Prim("+")), x58, P(P(P(S("n"), Code(x2)), N), P(Cell("global"), N)), mkCont[R]{(x59: R[Value]) =>
        x59: R[Value]
      })
      x60
    })(o.valueTag)
    x62
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
