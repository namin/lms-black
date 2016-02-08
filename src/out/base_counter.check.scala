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
        val x23 = o1._app(o1._lift(Prim("+")), x6,
        o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x7: R2[Value]) =>
            val o2 = implicitly[Ops[R2]]; implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x); val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x);
            val x8 = o2._cell_set(x2, x7)
            val x9 = o2._cell_read(x2)
            val x10 = o2._cons(x9, o2._lift(P(I(1), N)))
            val x21 = o2._app(o2._lift(Prim("+")), x10,
            o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x11: R3[Value]) =>
                val o3 = implicitly[Ops[R3]]; implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x); val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x);
                val x12 = o3._cell_set(x2, x11)
                val x13 = o3._cell_read(x2)
                val x14 = o3._cons(x13, o3._lift(P(I(1), N)))
                val x19 = o3._app(o3._lift(Prim("+")), x14,
                o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x15: R4[Value]) =>
                    val o4 = implicitly[Ops[R4]]; implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x); val ev2_4: Convert[R2,R4] = convertTrans[R2,R3,R4](ev2_3, ev3_4); implicit def convert_ev2_4(x: R2[Value]): R4[Value] = ev2_4.convert(x); val ev1_4: Convert[R1,R4] = convertTrans[R1,R2,R4](ev1_2, ev2_4); implicit def convert_ev1_4(x: R1[Value]): R4[Value] = ev1_4.convert(x); val ev_4: Convert[R,R4] = convertTrans[R,R1,R4](ev_1, ev1_4); implicit def convert_ev_4(x: R[Value]): R4[Value] = ev_4.convert(x);
                    val x16 = o4._cell_set(x2, x15)
                    val x17 = o4._cell_read(x2)
                    x17: R4[Value]
                }})
                )
                x19: R3[Value]
            }})
            )
            x21: R2[Value]
        }})
        )
        x23: R1[Value]
    }})
    x25
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
