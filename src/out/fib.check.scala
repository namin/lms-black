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
    val x4 = x1
    val x5 = o._cons(x4, o._lift(P(I(2), N)))
    val x37 = o._app(o._lift(Prim("<")), x5,
    o._cont(new FunC[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x6: R1[Value]) =>
        val o1 = implicitly[Ops[R1]]; implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x);
        val x7 = o1._true(x6)
        val x35 = o1._if((x7), {
          val x8 = x1
          x8
        }, {
          val x10 = o1._cell_read(o1._lift(Cell("fib")))
          val x11 = x1
          val x12 = o1._cons(x11, o1._lift(P(I(1), N)))
          val x33 = o1._app(o1._lift(Prim("-")), x12,
          o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x13: R2[Value]) =>
              val o2 = implicitly[Ops[R2]]; implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x); val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x);
              val x14 = o2._cons(x13, o2._lift(N))
              val x31 = o2._app(x10, x14,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x15: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]; implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x); val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x);
                  val x16 = o3._cell_read(o3._lift(Cell("fib")))
                  val x17 = x1
                  val x18 = o3._cons(x17, o3._lift(P(I(2), N)))
                  val x29 = o3._app(o3._lift(Prim("-")), x18,
                  o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x19: R4[Value]) =>
                      val o4 = implicitly[Ops[R4]]; implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x); val ev2_4: Convert[R2,R4] = convertTrans[R2,R3,R4](ev2_3, ev3_4); implicit def convert_ev2_4(x: R2[Value]): R4[Value] = ev2_4.convert(x); val ev1_4: Convert[R1,R4] = convertTrans[R1,R2,R4](ev1_2, ev2_4); implicit def convert_ev1_4(x: R1[Value]): R4[Value] = ev1_4.convert(x); val ev_4: Convert[R,R4] = convertTrans[R,R1,R4](ev_1, ev1_4); implicit def convert_ev_4(x: R[Value]): R4[Value] = ev_4.convert(x);
                      val x20 = o4._cons(x19, o4._lift(N))
                      val x27 = o4._app(x16, x20,
                      o4._cont(new FunC[R4] { def fun[R5[_]:Ops](implicit ev4_5: Convert[R4,R5]) = {(x21: R5[Value]) =>
                          val o5 = implicitly[Ops[R5]]; implicit def convert_ev4_5(x: R4[Value]): R5[Value] = ev4_5.convert(x); val ev3_5: Convert[R3,R5] = convertTrans[R3,R4,R5](ev3_4, ev4_5); implicit def convert_ev3_5(x: R3[Value]): R5[Value] = ev3_5.convert(x); val ev2_5: Convert[R2,R5] = convertTrans[R2,R3,R5](ev2_3, ev3_5); implicit def convert_ev2_5(x: R2[Value]): R5[Value] = ev2_5.convert(x); val ev1_5: Convert[R1,R5] = convertTrans[R1,R2,R5](ev1_2, ev2_5); implicit def convert_ev1_5(x: R1[Value]): R5[Value] = ev1_5.convert(x); val ev_5: Convert[R,R5] = convertTrans[R,R1,R5](ev_1, ev1_5); implicit def convert_ev_5(x: R[Value]): R5[Value] = ev_5.convert(x);
                          val x22 = o5._cons(x21, o5._lift(N))
                          val x23 = o5._cons(x15, x22)
                          val x25 = o5._app(o5._lift(Prim("+")), x23,
                          o5._cont(new FunC[R5] { def fun[R6[_]:Ops](implicit ev5_6: Convert[R5,R6]) = {(x24: R6[Value]) =>
                              x24: R6[Value]
                          }})
                          )
                          x25: R5[Value]
                      }})
                      )
                      x27: R4[Value]
                  }})
                  )
                  x29: R3[Value]
              }})
              )
              x31: R2[Value]
          }})
          )
          x33
        })(o1.valueTag)
        x35: R1[Value]
    }})
    )
    x37
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
