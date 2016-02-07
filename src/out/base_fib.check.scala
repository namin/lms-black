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
    val x1 = o._car(x0)
    val x2 = x1
    val x43 = o._fun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1._car(x4)
        val x6 = x5
        val x8 = x5
        val x9 = o1._cons(x8, o1._lift(P(I(2), N)))
        val x41 = o1._app(o1._lift(Prim("<")), x9, mkCont[R1]{(x10: R1[Value]) =>
          val x11 = o1._true(x10)
          val x39 = o1._if((x11), {
            val x12 = x5
            x12
          }, {
            val x14 = x1
            val x15 = x5
            val x16 = o1._cons(x15, o1._lift(P(I(1), N)))
            val x37 = o1._app(o1._lift(Prim("-")), x16, mkCont[R1]{(x17: R1[Value]) =>
              val x18 = o1._cons(x17, o1._lift(N))
              val x35 = o1._app(x14, x18, mkCont[R1]{(x19: R1[Value]) =>
                val x20 = x1
                val x21 = x5
                val x22 = o1._cons(x21, o1._lift(P(I(2), N)))
                val x33 = o1._app(o1._lift(Prim("-")), x22, mkCont[R1]{(x23: R1[Value]) =>
                  val x24 = o1._cons(x23, o1._lift(N))
                  val x31 = o1._app(x20, x24, mkCont[R1]{(x25: R1[Value]) =>
                    val x26 = o1._cons(x25, o1._lift(N))
                    val x27 = o1._cons(x19, x26)
                    val x29 = o1._app(o1._lift(Prim("+")), x27, mkCont[R1]{(x28: R1[Value]) =>
                      x28: R1[Value]
                    })
                    x29: R1[Value]
                  })
                  x31: R1[Value]
                })
                x33: R1[Value]
              })
              x35: R1[Value]
            })
            x37
          })(o1.valueTag)
          x39: R1[Value]
        })
        x41: R1[Value]
    }})
    x43
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
