/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import language.implicitConversions
import scala.lms.black.eval._
class staged$0 extends Fun[NoRep] with (Value => Value) {
  def apply(v: Value): Value = fun[NoRep](v)(OpsNoRep, convertSame[NoRep])
  def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { v => fun[R](v)(implicitly[Ops[R]], ev)  }
  def fun[R[_]:Ops](x0:Value)(implicit ev: Convert[NoRep,R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = o._cdr(x0)
    val x2 = o._car(x1)
    val x3 = x2
    val x5 = x2
    val x6 = o._cons(x5, o._lift(P(I(2), N)))
    val x10 = o._car(x0)
    val x46 = o._app(o._lift(Prim("<")), x6,
    o._cont(new FunC[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x7: R1[Value]) =>
        val o1 = implicitly[Ops[R1]]; implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x); implicit def convertb_ev_1(x: R[Boolean]): R1[Boolean] = ev_1.convertb(x);
        val x8 = o1._true(x7)
        val x44 = o1._if((x8), {
          val x9 = x2
          val x11 = o1._cons(x9, o1._lift(N))
          val x13 = o1._app(x10, x11,
          o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x12: R2[Value]) =>
              x12: R2[Value]
          }})
          )
          x13
        }, {
          val x15 = o1._cell_read(o1._lift(Cell("fib")))
          val x16 = x2
          val x17 = o1._cons(x16, o1._lift(P(I(1), N)))
          val x42 = o1._app(o1._lift(Prim("-")), x17,
          o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x18: R2[Value]) =>
              val o2 = implicitly[Ops[R2]]; implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x); implicit def convertb_ev1_2(x: R1[Boolean]): R2[Boolean] = ev1_2.convertb(x); val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x); implicit def convertb_ev_2(x: R[Boolean]): R2[Boolean] = ev_2.convertb(x);
              val x19 = o2._cons(x18, o2._lift(N))
              val x40 = o2._app(x15, x19,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x20: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]; implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x); implicit def convertb_ev2_3(x: R2[Boolean]): R3[Boolean] = ev2_3.convertb(x); val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); implicit def convertb_ev1_3(x: R1[Boolean]): R3[Boolean] = ev1_3.convertb(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x); implicit def convertb_ev_3(x: R[Boolean]): R3[Boolean] = ev_3.convertb(x);
                  val x21 = o3._cell_read(o3._lift(Cell("fib")))
                  val x22 = x2
                  val x23 = o3._cons(x22, o3._lift(P(I(2), N)))
                  val x38 = o3._app(o3._lift(Prim("-")), x23,
                  o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x24: R4[Value]) =>
                      val o4 = implicitly[Ops[R4]]; implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x); implicit def convertb_ev3_4(x: R3[Boolean]): R4[Boolean] = ev3_4.convertb(x); val ev2_4: Convert[R2,R4] = convertTrans[R2,R3,R4](ev2_3, ev3_4); implicit def convert_ev2_4(x: R2[Value]): R4[Value] = ev2_4.convert(x); implicit def convertb_ev2_4(x: R2[Boolean]): R4[Boolean] = ev2_4.convertb(x); val ev1_4: Convert[R1,R4] = convertTrans[R1,R2,R4](ev1_2, ev2_4); implicit def convert_ev1_4(x: R1[Value]): R4[Value] = ev1_4.convert(x); implicit def convertb_ev1_4(x: R1[Boolean]): R4[Boolean] = ev1_4.convertb(x); val ev_4: Convert[R,R4] = convertTrans[R,R1,R4](ev_1, ev1_4); implicit def convert_ev_4(x: R[Value]): R4[Value] = ev_4.convert(x); implicit def convertb_ev_4(x: R[Boolean]): R4[Boolean] = ev_4.convertb(x);
                      val x25 = o4._cons(x24, o4._lift(N))
                      val x36 = o4._app(x21, x25,
                      o4._cont(new FunC[R4] { def fun[R5[_]:Ops](implicit ev4_5: Convert[R4,R5]) = {(x26: R5[Value]) =>
                          val o5 = implicitly[Ops[R5]]; implicit def convert_ev4_5(x: R4[Value]): R5[Value] = ev4_5.convert(x); implicit def convertb_ev4_5(x: R4[Boolean]): R5[Boolean] = ev4_5.convertb(x); val ev3_5: Convert[R3,R5] = convertTrans[R3,R4,R5](ev3_4, ev4_5); implicit def convert_ev3_5(x: R3[Value]): R5[Value] = ev3_5.convert(x); implicit def convertb_ev3_5(x: R3[Boolean]): R5[Boolean] = ev3_5.convertb(x); val ev2_5: Convert[R2,R5] = convertTrans[R2,R3,R5](ev2_3, ev3_5); implicit def convert_ev2_5(x: R2[Value]): R5[Value] = ev2_5.convert(x); implicit def convertb_ev2_5(x: R2[Boolean]): R5[Boolean] = ev2_5.convertb(x); val ev1_5: Convert[R1,R5] = convertTrans[R1,R2,R5](ev1_2, ev2_5); implicit def convert_ev1_5(x: R1[Value]): R5[Value] = ev1_5.convert(x); implicit def convertb_ev1_5(x: R1[Boolean]): R5[Boolean] = ev1_5.convertb(x); val ev_5: Convert[R,R5] = convertTrans[R,R1,R5](ev_1, ev1_5); implicit def convert_ev_5(x: R[Value]): R5[Value] = ev_5.convert(x); implicit def convertb_ev_5(x: R[Boolean]): R5[Boolean] = ev_5.convertb(x);
                          val x27 = o5._cons(x26, o5._lift(N))
                          val x28 = o5._cons(x20, x27)
                          val x34 = o5._app(o5._lift(Prim("+")), x28,
                          o5._cont(new FunC[R5] { def fun[R6[_]:Ops](implicit ev5_6: Convert[R5,R6]) = {(x29: R6[Value]) =>
                              val o6 = implicitly[Ops[R6]]; implicit def convert_ev5_6(x: R5[Value]): R6[Value] = ev5_6.convert(x); implicit def convertb_ev5_6(x: R5[Boolean]): R6[Boolean] = ev5_6.convertb(x); val ev4_6: Convert[R4,R6] = convertTrans[R4,R5,R6](ev4_5, ev5_6); implicit def convert_ev4_6(x: R4[Value]): R6[Value] = ev4_6.convert(x); implicit def convertb_ev4_6(x: R4[Boolean]): R6[Boolean] = ev4_6.convertb(x); val ev3_6: Convert[R3,R6] = convertTrans[R3,R4,R6](ev3_4, ev4_6); implicit def convert_ev3_6(x: R3[Value]): R6[Value] = ev3_6.convert(x); implicit def convertb_ev3_6(x: R3[Boolean]): R6[Boolean] = ev3_6.convertb(x); val ev2_6: Convert[R2,R6] = convertTrans[R2,R3,R6](ev2_3, ev3_6); implicit def convert_ev2_6(x: R2[Value]): R6[Value] = ev2_6.convert(x); implicit def convertb_ev2_6(x: R2[Boolean]): R6[Boolean] = ev2_6.convertb(x); val ev1_6: Convert[R1,R6] = convertTrans[R1,R2,R6](ev1_2, ev2_6); implicit def convert_ev1_6(x: R1[Value]): R6[Value] = ev1_6.convert(x); implicit def convertb_ev1_6(x: R1[Boolean]): R6[Boolean] = ev1_6.convertb(x); val ev_6: Convert[R,R6] = convertTrans[R,R1,R6](ev_1, ev1_6); implicit def convert_ev_6(x: R[Value]): R6[Value] = ev_6.convert(x); implicit def convertb_ev_6(x: R[Boolean]): R6[Boolean] = ev_6.convertb(x);
                              val x30 = o6._cons(x29, o6._lift(N))
                              val x32 = o6._app(x10, x30,
                              o6._cont(new FunC[R6] { def fun[R7[_]:Ops](implicit ev6_7: Convert[R6,R7]) = {(x31: R7[Value]) =>
                                  x31: R7[Value]
                              }})
                              )
                              x32: R6[Value]
                          }})
                          )
                          x34: R5[Value]
                      }})
                      )
                      x36: R4[Value]
                  }})
                  )
                  x38: R3[Value]
              }})
              )
              x40: R2[Value]
          }})
          )
          x42
        })(o1.valueTag)
        x44: R1[Value]
    }})
    )
    x46
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
