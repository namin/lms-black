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
    val x2 = o._cell_new(x1, "c")
    val x25 = o._fun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1._cell_read(x2)
        val x6 = o1._cons(x5, o1._lift(P(I(1), N)))
        val x23 = o1._app(o1._lift(Prim("+")), x6, mkCont[R1]{(x7: R1[Value]) =>
          val x8 = o1._cell_set(x2, x7)
          val x9 = o1._cell_read(x2)
          val x10 = o1._cons(x9, o1._lift(P(I(1), N)))
          val x21 = o1._app(o1._lift(Prim("+")), x10, mkCont[R1]{(x11: R1[Value]) =>
            val x12 = o1._cell_set(x2, x11)
            val x13 = o1._cell_read(x2)
            val x14 = o1._cons(x13, o1._lift(P(I(1), N)))
            val x19 = o1._app(o1._lift(Prim("+")), x14, mkCont[R1]{(x15: R1[Value]) =>
              val x16 = o1._cell_set(x2, x15)
              val x17 = o1._cell_read(x2)
              x17: R1[Value]
            })
            x19: R1[Value]
          })
          x21: R1[Value]
        })
        x23: R1[Value]
    }})
    x25
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
