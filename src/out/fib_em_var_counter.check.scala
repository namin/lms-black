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
    val x1 = o._cdr(x0)
    val x2 = o._car(x1)
    val x3 = x2
    val x6 = o._cell_read(o._lift(Cell("counter")))
    val x7 = o._cons(x6, o._lift(P(I(1), N)))
    val x21 = o._car(x0)
    val x86 = o._app(o._lift(Prim("+")), x7,
    o._cont(new FunC[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x8: R1[Value]) =>
        val o1 = implicitly[Ops[R1]]; implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x); implicit def convertb_ev_1(x: R[Boolean]): R1[Boolean] = ev_1.convertb(x);
        val x9 = o1._cell_set(o1._lift(Cell("counter")), x8)
        val x11 = x2
        val x12 = o1._cons(x11, o1._lift(P(I(2), N)))
        val x83 = o1._app(o1._lift(Prim("<")), x12,
        o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x13: R2[Value]) =>
            val o2 = implicitly[Ops[R2]]; implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x); implicit def convertb_ev1_2(x: R1[Boolean]): R2[Boolean] = ev1_2.convertb(x); val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x); implicit def convertb_ev_2(x: R[Boolean]): R2[Boolean] = ev_2.convertb(x);
            val x14 = o2._true(x13)
            val x81 = o2._if((x14), {
              val x15 = o2._cell_read(o2._lift(Cell("counter")))
              val x16 = o2._cons(x15, o2._lift(P(I(1), N)))
              val x27 = o2._app(o2._lift(Prim("+")), x16,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x17: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]; implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x); implicit def convertb_ev2_3(x: R2[Boolean]): R3[Boolean] = ev2_3.convertb(x); val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); implicit def convertb_ev1_3(x: R1[Boolean]): R3[Boolean] = ev1_3.convertb(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x); implicit def convertb_ev_3(x: R[Boolean]): R3[Boolean] = ev_3.convertb(x);
                  val x18 = o3._cell_set(o3._lift(Cell("counter")), x17)
                  val x20 = x2
                  val x22 = o3._cons(x20, o3._lift(N))
                  val x24 = o3._app(x21, x22,
                  o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x23: R4[Value]) =>
                      x23: R4[Value]
                  }})
                  )
                  x24: R3[Value]
              }})
              )
              x27
            }, {
              val x31 = o2._cell_read(o2._lift(Cell("fib")))
              val x33 = o2._cell_read(o2._lift(Cell("counter")))
              val x34 = o2._cons(x33, o2._lift(P(I(1), N)))
              val x78 = o2._app(o2._lift(Prim("+")), x34,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x35: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]; implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x); implicit def convertb_ev2_3(x: R2[Boolean]): R3[Boolean] = ev2_3.convertb(x); val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); implicit def convertb_ev1_3(x: R1[Boolean]): R3[Boolean] = ev1_3.convertb(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x); implicit def convertb_ev_3(x: R[Boolean]): R3[Boolean] = ev_3.convertb(x);
                  val x36 = o3._cell_set(o3._lift(Cell("counter")), x35)
                  val x38 = x2
                  val x39 = o3._cons(x38, o3._lift(P(I(1), N)))
                  val x75 = o3._app(o3._lift(Prim("-")), x39,
                  o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x40: R4[Value]) =>
                      val o4 = implicitly[Ops[R4]]; implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x); implicit def convertb_ev3_4(x: R3[Boolean]): R4[Boolean] = ev3_4.convertb(x); val ev2_4: Convert[R2,R4] = convertTrans[R2,R3,R4](ev2_3, ev3_4); implicit def convert_ev2_4(x: R2[Value]): R4[Value] = ev2_4.convert(x); implicit def convertb_ev2_4(x: R2[Boolean]): R4[Boolean] = ev2_4.convertb(x); val ev1_4: Convert[R1,R4] = convertTrans[R1,R2,R4](ev1_2, ev2_4); implicit def convert_ev1_4(x: R1[Value]): R4[Value] = ev1_4.convert(x); implicit def convertb_ev1_4(x: R1[Boolean]): R4[Boolean] = ev1_4.convertb(x); val ev_4: Convert[R,R4] = convertTrans[R,R1,R4](ev_1, ev1_4); implicit def convert_ev_4(x: R[Value]): R4[Value] = ev_4.convert(x); implicit def convertb_ev_4(x: R[Boolean]): R4[Boolean] = ev_4.convertb(x);
                      val x41 = o4._cons(x40, o4._lift(N))
                      val x73 = o4._app(x31, x41,
                      o4._cont(new FunC[R4] { def fun[R5[_]:Ops](implicit ev4_5: Convert[R4,R5]) = {(x42: R5[Value]) =>
                          val o5 = implicitly[Ops[R5]]; implicit def convert_ev4_5(x: R4[Value]): R5[Value] = ev4_5.convert(x); implicit def convertb_ev4_5(x: R4[Boolean]): R5[Boolean] = ev4_5.convertb(x); val ev3_5: Convert[R3,R5] = convertTrans[R3,R4,R5](ev3_4, ev4_5); implicit def convert_ev3_5(x: R3[Value]): R5[Value] = ev3_5.convert(x); implicit def convertb_ev3_5(x: R3[Boolean]): R5[Boolean] = ev3_5.convertb(x); val ev2_5: Convert[R2,R5] = convertTrans[R2,R3,R5](ev2_3, ev3_5); implicit def convert_ev2_5(x: R2[Value]): R5[Value] = ev2_5.convert(x); implicit def convertb_ev2_5(x: R2[Boolean]): R5[Boolean] = ev2_5.convertb(x); val ev1_5: Convert[R1,R5] = convertTrans[R1,R2,R5](ev1_2, ev2_5); implicit def convert_ev1_5(x: R1[Value]): R5[Value] = ev1_5.convert(x); implicit def convertb_ev1_5(x: R1[Boolean]): R5[Boolean] = ev1_5.convertb(x); val ev_5: Convert[R,R5] = convertTrans[R,R1,R5](ev_1, ev1_5); implicit def convert_ev_5(x: R[Value]): R5[Value] = ev_5.convert(x); implicit def convertb_ev_5(x: R[Boolean]): R5[Boolean] = ev_5.convertb(x);
                          val x44 = o5._cell_read(o5._lift(Cell("fib")))
                          val x46 = o5._cell_read(o5._lift(Cell("counter")))
                          val x47 = o5._cons(x46, o5._lift(P(I(1), N)))
                          val x70 = o5._app(o5._lift(Prim("+")), x47,
                          o5._cont(new FunC[R5] { def fun[R6[_]:Ops](implicit ev5_6: Convert[R5,R6]) = {(x48: R6[Value]) =>
                              val o6 = implicitly[Ops[R6]]; implicit def convert_ev5_6(x: R5[Value]): R6[Value] = ev5_6.convert(x); implicit def convertb_ev5_6(x: R5[Boolean]): R6[Boolean] = ev5_6.convertb(x); val ev4_6: Convert[R4,R6] = convertTrans[R4,R5,R6](ev4_5, ev5_6); implicit def convert_ev4_6(x: R4[Value]): R6[Value] = ev4_6.convert(x); implicit def convertb_ev4_6(x: R4[Boolean]): R6[Boolean] = ev4_6.convertb(x); val ev3_6: Convert[R3,R6] = convertTrans[R3,R4,R6](ev3_4, ev4_6); implicit def convert_ev3_6(x: R3[Value]): R6[Value] = ev3_6.convert(x); implicit def convertb_ev3_6(x: R3[Boolean]): R6[Boolean] = ev3_6.convertb(x); val ev2_6: Convert[R2,R6] = convertTrans[R2,R3,R6](ev2_3, ev3_6); implicit def convert_ev2_6(x: R2[Value]): R6[Value] = ev2_6.convert(x); implicit def convertb_ev2_6(x: R2[Boolean]): R6[Boolean] = ev2_6.convertb(x); val ev1_6: Convert[R1,R6] = convertTrans[R1,R2,R6](ev1_2, ev2_6); implicit def convert_ev1_6(x: R1[Value]): R6[Value] = ev1_6.convert(x); implicit def convertb_ev1_6(x: R1[Boolean]): R6[Boolean] = ev1_6.convertb(x); val ev_6: Convert[R,R6] = convertTrans[R,R1,R6](ev_1, ev1_6); implicit def convert_ev_6(x: R[Value]): R6[Value] = ev_6.convert(x); implicit def convertb_ev_6(x: R[Boolean]): R6[Boolean] = ev_6.convertb(x);
                              val x49 = o6._cell_set(o6._lift(Cell("counter")), x48)
                              val x51 = x2
                              val x52 = o6._cons(x51, o6._lift(P(I(2), N)))
                              val x67 = o6._app(o6._lift(Prim("-")), x52,
                              o6._cont(new FunC[R6] { def fun[R7[_]:Ops](implicit ev6_7: Convert[R6,R7]) = {(x53: R7[Value]) =>
                                  val o7 = implicitly[Ops[R7]]; implicit def convert_ev6_7(x: R6[Value]): R7[Value] = ev6_7.convert(x); implicit def convertb_ev6_7(x: R6[Boolean]): R7[Boolean] = ev6_7.convertb(x); val ev5_7: Convert[R5,R7] = convertTrans[R5,R6,R7](ev5_6, ev6_7); implicit def convert_ev5_7(x: R5[Value]): R7[Value] = ev5_7.convert(x); implicit def convertb_ev5_7(x: R5[Boolean]): R7[Boolean] = ev5_7.convertb(x); val ev4_7: Convert[R4,R7] = convertTrans[R4,R5,R7](ev4_5, ev5_7); implicit def convert_ev4_7(x: R4[Value]): R7[Value] = ev4_7.convert(x); implicit def convertb_ev4_7(x: R4[Boolean]): R7[Boolean] = ev4_7.convertb(x); val ev3_7: Convert[R3,R7] = convertTrans[R3,R4,R7](ev3_4, ev4_7); implicit def convert_ev3_7(x: R3[Value]): R7[Value] = ev3_7.convert(x); implicit def convertb_ev3_7(x: R3[Boolean]): R7[Boolean] = ev3_7.convertb(x); val ev2_7: Convert[R2,R7] = convertTrans[R2,R3,R7](ev2_3, ev3_7); implicit def convert_ev2_7(x: R2[Value]): R7[Value] = ev2_7.convert(x); implicit def convertb_ev2_7(x: R2[Boolean]): R7[Boolean] = ev2_7.convertb(x); val ev1_7: Convert[R1,R7] = convertTrans[R1,R2,R7](ev1_2, ev2_7); implicit def convert_ev1_7(x: R1[Value]): R7[Value] = ev1_7.convert(x); implicit def convertb_ev1_7(x: R1[Boolean]): R7[Boolean] = ev1_7.convertb(x); val ev_7: Convert[R,R7] = convertTrans[R,R1,R7](ev_1, ev1_7); implicit def convert_ev_7(x: R[Value]): R7[Value] = ev_7.convert(x); implicit def convertb_ev_7(x: R[Boolean]): R7[Boolean] = ev_7.convertb(x);
                                  val x54 = o7._cons(x53, o7._lift(N))
                                  val x65 = o7._app(x44, x54,
                                  o7._cont(new FunC[R7] { def fun[R8[_]:Ops](implicit ev7_8: Convert[R7,R8]) = {(x55: R8[Value]) =>
                                      val o8 = implicitly[Ops[R8]]; implicit def convert_ev7_8(x: R7[Value]): R8[Value] = ev7_8.convert(x); implicit def convertb_ev7_8(x: R7[Boolean]): R8[Boolean] = ev7_8.convertb(x); val ev6_8: Convert[R6,R8] = convertTrans[R6,R7,R8](ev6_7, ev7_8); implicit def convert_ev6_8(x: R6[Value]): R8[Value] = ev6_8.convert(x); implicit def convertb_ev6_8(x: R6[Boolean]): R8[Boolean] = ev6_8.convertb(x); val ev5_8: Convert[R5,R8] = convertTrans[R5,R6,R8](ev5_6, ev6_8); implicit def convert_ev5_8(x: R5[Value]): R8[Value] = ev5_8.convert(x); implicit def convertb_ev5_8(x: R5[Boolean]): R8[Boolean] = ev5_8.convertb(x); val ev4_8: Convert[R4,R8] = convertTrans[R4,R5,R8](ev4_5, ev5_8); implicit def convert_ev4_8(x: R4[Value]): R8[Value] = ev4_8.convert(x); implicit def convertb_ev4_8(x: R4[Boolean]): R8[Boolean] = ev4_8.convertb(x); val ev3_8: Convert[R3,R8] = convertTrans[R3,R4,R8](ev3_4, ev4_8); implicit def convert_ev3_8(x: R3[Value]): R8[Value] = ev3_8.convert(x); implicit def convertb_ev3_8(x: R3[Boolean]): R8[Boolean] = ev3_8.convertb(x); val ev2_8: Convert[R2,R8] = convertTrans[R2,R3,R8](ev2_3, ev3_8); implicit def convert_ev2_8(x: R2[Value]): R8[Value] = ev2_8.convert(x); implicit def convertb_ev2_8(x: R2[Boolean]): R8[Boolean] = ev2_8.convertb(x); val ev1_8: Convert[R1,R8] = convertTrans[R1,R2,R8](ev1_2, ev2_8); implicit def convert_ev1_8(x: R1[Value]): R8[Value] = ev1_8.convert(x); implicit def convertb_ev1_8(x: R1[Boolean]): R8[Boolean] = ev1_8.convertb(x); val ev_8: Convert[R,R8] = convertTrans[R,R1,R8](ev_1, ev1_8); implicit def convert_ev_8(x: R[Value]): R8[Value] = ev_8.convert(x); implicit def convertb_ev_8(x: R[Boolean]): R8[Boolean] = ev_8.convertb(x);
                                      val x56 = o8._cons(x55, o8._lift(N))
                                      val x57 = o8._cons(x42, x56)
                                      val x63 = o8._app(o8._lift(Prim("+")), x57,
                                      o8._cont(new FunC[R8] { def fun[R9[_]:Ops](implicit ev8_9: Convert[R8,R9]) = {(x58: R9[Value]) =>
                                          val o9 = implicitly[Ops[R9]]; implicit def convert_ev8_9(x: R8[Value]): R9[Value] = ev8_9.convert(x); implicit def convertb_ev8_9(x: R8[Boolean]): R9[Boolean] = ev8_9.convertb(x); val ev7_9: Convert[R7,R9] = convertTrans[R7,R8,R9](ev7_8, ev8_9); implicit def convert_ev7_9(x: R7[Value]): R9[Value] = ev7_9.convert(x); implicit def convertb_ev7_9(x: R7[Boolean]): R9[Boolean] = ev7_9.convertb(x); val ev6_9: Convert[R6,R9] = convertTrans[R6,R7,R9](ev6_7, ev7_9); implicit def convert_ev6_9(x: R6[Value]): R9[Value] = ev6_9.convert(x); implicit def convertb_ev6_9(x: R6[Boolean]): R9[Boolean] = ev6_9.convertb(x); val ev5_9: Convert[R5,R9] = convertTrans[R5,R6,R9](ev5_6, ev6_9); implicit def convert_ev5_9(x: R5[Value]): R9[Value] = ev5_9.convert(x); implicit def convertb_ev5_9(x: R5[Boolean]): R9[Boolean] = ev5_9.convertb(x); val ev4_9: Convert[R4,R9] = convertTrans[R4,R5,R9](ev4_5, ev5_9); implicit def convert_ev4_9(x: R4[Value]): R9[Value] = ev4_9.convert(x); implicit def convertb_ev4_9(x: R4[Boolean]): R9[Boolean] = ev4_9.convertb(x); val ev3_9: Convert[R3,R9] = convertTrans[R3,R4,R9](ev3_4, ev4_9); implicit def convert_ev3_9(x: R3[Value]): R9[Value] = ev3_9.convert(x); implicit def convertb_ev3_9(x: R3[Boolean]): R9[Boolean] = ev3_9.convertb(x); val ev2_9: Convert[R2,R9] = convertTrans[R2,R3,R9](ev2_3, ev3_9); implicit def convert_ev2_9(x: R2[Value]): R9[Value] = ev2_9.convert(x); implicit def convertb_ev2_9(x: R2[Boolean]): R9[Boolean] = ev2_9.convertb(x); val ev1_9: Convert[R1,R9] = convertTrans[R1,R2,R9](ev1_2, ev2_9); implicit def convert_ev1_9(x: R1[Value]): R9[Value] = ev1_9.convert(x); implicit def convertb_ev1_9(x: R1[Boolean]): R9[Boolean] = ev1_9.convertb(x); val ev_9: Convert[R,R9] = convertTrans[R,R1,R9](ev_1, ev1_9); implicit def convert_ev_9(x: R[Value]): R9[Value] = ev_9.convert(x); implicit def convertb_ev_9(x: R[Boolean]): R9[Boolean] = ev_9.convertb(x);
                                          val x59 = o9._cons(x58, o9._lift(N))
                                          val x61 = o9._app(x21, x59,
                                          o9._cont(new FunC[R9] { def fun[R10[_]:Ops](implicit ev9_10: Convert[R9,R10]) = {(x60: R10[Value]) =>
                                              x60: R10[Value]
                                          }})
                                          )
                                          x61: R9[Value]
                                      }})
                                      )
                                      x63: R8[Value]
                                  }})
                                  )
                                  x65: R7[Value]
                              }})
                              )
                              x67: R6[Value]
                          }})
                          )
                          x70: R5[Value]
                      }})
                      )
                      x73: R4[Value]
                  }})
                  )
                  x75: R3[Value]
              }})
              )
              x78
            })(o2.valueTag)
            x81: R2[Value]
        }})
        )
        x83: R1[Value]
    }})
    )
    x86
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
