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
    val x14 = o._fun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x4: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x5 = o1._car(x4)
        val x6 = x5
        val x8 = x5
        val x9 = x5
        val x10 = o1._cons(x9, o1._lift(N))
        val x12 = o1._app(x8, x10,
        o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x11: R2[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            x11: R2[Value]
        }})
        )
        x12: R1[Value]
    }})
    val x40 = o._fun(new Fun[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x15: R[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x16 = o1._car(x15)
        val x17 = x16
        val x19 = x1
        val x35 = o1._fun(new Fun[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x20: R1[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x);
            val x21 = o2._car(x20)
            val x22 = x21
            val x24 = x16
            val x25 = x16
            val x26 = o2._cons(x25, o2._lift(N))
            val x33 = o2._app(x24, x26,
            o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x27: R3[Value]) =>
                val o3 = implicitly[Ops[R3]]
                implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x)
                val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x);
                val x28 = x21
                val x29 = o3._cons(x28, o3._lift(N))
                val x31 = o3._app(x27, x29,
                o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x30: R4[Value]) =>
                    val o4 = implicitly[Ops[R4]]
                    implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x)
                    x30: R4[Value]
                }})
                )
                x31: R3[Value]
            }})
            )
            x33: R2[Value]
        }})
        val x36 = o1._cons(x35, o1._lift(N))
        val x38 = o1._app(x19, x36,
        o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x37: R2[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            x37: R2[Value]
        }})
        )
        x38: R1[Value]
    }})
    val x41 = o._cons(x40, o._lift(N))
    val x43 = o._app(x14, x41,
    o._cont(new FunC[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x42: R1[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        x42: R1[Value]
    }})
    )
    x43
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
