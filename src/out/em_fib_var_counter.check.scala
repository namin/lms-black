/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import language.implicitConversions
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
    val x71 = o.app(o.lift(Prim("+")), x6, mkCont[R]{(x7: R[Value]) =>
      val x8 = o.cellSet(o.lift(Cell("counter")), x7)
      val x10 = x1
      val x11 = o.makePair(x10, o.lift(P(I(2), N)))
      val x69 = o.app(o.lift(Prim("<")), x11, mkCont[R]{(x12: R[Value]) =>
        val x13 = o.isTrue(x12)
        val x67 = o.ifThenElse((x13), {
          val x14 = o.cellRead(o.lift(Cell("counter")))
          val x15 = o.makePair(x14, o.lift(P(I(1), N)))
          val x21 = o.app(o.lift(Prim("+")), x15, mkCont[R]{(x16: R[Value]) =>
            val x17 = o.cellSet(o.lift(Cell("counter")), x16)
            val x19 = x1
            x19: R[Value]
          })
          x21
        }, {
          val x25 = o.cellRead(o.lift(Cell("fib")))
          val x27 = o.cellRead(o.lift(Cell("counter")))
          val x28 = o.makePair(x27, o.lift(P(I(1), N)))
          val x65 = o.app(o.lift(Prim("+")), x28, mkCont[R]{(x29: R[Value]) =>
            val x30 = o.cellSet(o.lift(Cell("counter")), x29)
            val x32 = x1
            val x33 = o.makePair(x32, o.lift(P(I(1), N)))
            val x63 = o.app(o.lift(Prim("-")), x33, mkCont[R]{(x34: R[Value]) =>
              val x35 = o.makePair(x34, o.lift(N))
              val x61 = o.app(x25, x35, mkCont[R]{(x36: R[Value]) =>
                val x38 = o.cellRead(o.lift(Cell("fib")))
                val x40 = o.cellRead(o.lift(Cell("counter")))
                val x41 = o.makePair(x40, o.lift(P(I(1), N)))
                val x59 = o.app(o.lift(Prim("+")), x41, mkCont[R]{(x42: R[Value]) =>
                  val x43 = o.cellSet(o.lift(Cell("counter")), x42)
                  val x45 = x1
                  val x46 = o.makePair(x45, o.lift(P(I(2), N)))
                  val x57 = o.app(o.lift(Prim("-")), x46, mkCont[R]{(x47: R[Value]) =>
                    val x48 = o.makePair(x47, o.lift(N))
                    val x55 = o.app(x38, x48, mkCont[R]{(x49: R[Value]) =>
                      val x50 = o.makePair(x49, o.lift(N))
                      val x51 = o.makePair(x36, x50)
                      val x53 = o.app(o.lift(Prim("+")), x51, mkCont[R]{(x52: R[Value]) =>
                        x52: R[Value]
                      })
                      x53: R[Value]
                    })
                    x55: R[Value]
                  })
                  x57: R[Value]
                })
                x59: R[Value]
              })
              x61: R[Value]
            })
            x63: R[Value]
          })
          x65
        })(o.valueTag)
        x67: R[Value]
      })
      x69: R[Value]
    })
    x71
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
