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
    val x8152 = o._cell_read(o._lift(Cell("counter")))
    val x8153 = o._cons(x8152, o._lift(P(I(1), N)))
    val x10869 = o._app(o._lift(Prim("+")), x8153,
    o._cont(new FunC[R] { def fun[R1[_]:Ops](implicit ev_1: Convert[R,R1]) = {(x8154: R1[Value]) =>
        val o1 = implicitly[Ops[R1]]
        implicit def convert_ev_1(x: R[Value]): R1[Value] = ev_1.convert(x)
        val x8155 = o1._cell_set(o1._lift(Cell("counter")), x8154)
        val x8157 = x1
        val x8158 = o1._cons(x8157, o1._lift(P(I(2), N)))
        val x10867 = o1._app(o1._lift(Prim("<")), x8158,
        o1._cont(new FunC[R1] { def fun[R2[_]:Ops](implicit ev1_2: Convert[R1,R2]) = {(x8159: R2[Value]) =>
            val o2 = implicitly[Ops[R2]]
            implicit def convert_ev1_2(x: R1[Value]): R2[Value] = ev1_2.convert(x)
            val ev_2: Convert[R,R2] = convertTrans[R,R1,R2](ev_1, ev1_2); implicit def convert_ev_2(x: R[Value]): R2[Value] = ev_2.convert(x);
            val x8160 = o2._true(x8159)
            val x10865 = o2._if((x8160), {
              val x8161 = o2._cell_read(o2._lift(Cell("counter")))
              val x8162 = o2._cons(x8161, o2._lift(P(I(1), N)))
              val x8168 = o2._app(o2._lift(Prim("+")), x8162,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x8163: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]
                  implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x)
                  val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x);
                  val x8164 = o3._cell_set(o3._lift(Cell("counter")), x8163)
                  val x8166 = x1
                  x8166: R3[Value]
              }})
              )
              x8168
            }, {
              val x10193 = o2._cell_read(o2._lift(Cell("fib")))
              val x10695 = o2._cell_read(o2._lift(Cell("counter")))
              val x10696 = o2._cons(x10695, o2._lift(P(I(1), N)))
              val x10863 = o2._app(o2._lift(Prim("+")), x10696,
              o2._cont(new FunC[R2] { def fun[R3[_]:Ops](implicit ev2_3: Convert[R2,R3]) = {(x10697: R3[Value]) =>
                  val o3 = implicitly[Ops[R3]]
                  implicit def convert_ev2_3(x: R2[Value]): R3[Value] = ev2_3.convert(x)
                  val ev1_3: Convert[R1,R3] = convertTrans[R1,R2,R3](ev1_2, ev2_3); implicit def convert_ev1_3(x: R1[Value]): R3[Value] = ev1_3.convert(x); val ev_3: Convert[R,R3] = convertTrans[R,R1,R3](ev_1, ev1_3); implicit def convert_ev_3(x: R[Value]): R3[Value] = ev_3.convert(x);
                  val x10698 = o3._cell_set(o3._lift(Cell("counter")), x10697)
                  val x10700 = x1
                  val x10701 = o3._cons(x10700, o3._lift(P(I(1), N)))
                  val x10861 = o3._app(o3._lift(Prim("-")), x10701,
                  o3._cont(new FunC[R3] { def fun[R4[_]:Ops](implicit ev3_4: Convert[R3,R4]) = {(x10702: R4[Value]) =>
                      val o4 = implicitly[Ops[R4]]
                      implicit def convert_ev3_4(x: R3[Value]): R4[Value] = ev3_4.convert(x)
                      val ev2_4: Convert[R2,R4] = convertTrans[R2,R3,R4](ev2_3, ev3_4); implicit def convert_ev2_4(x: R2[Value]): R4[Value] = ev2_4.convert(x); val ev1_4: Convert[R1,R4] = convertTrans[R1,R2,R4](ev1_2, ev2_4); implicit def convert_ev1_4(x: R1[Value]): R4[Value] = ev1_4.convert(x); val ev_4: Convert[R,R4] = convertTrans[R,R1,R4](ev_1, ev1_4); implicit def convert_ev_4(x: R[Value]): R4[Value] = ev_4.convert(x);
                      val x10703 = o4._cons(x10702, o4._lift(N))
                      val x10859 = o4._app(x10193, x10703,
                      o4._cont(new FunC[R4] { def fun[R5[_]:Ops](implicit ev4_5: Convert[R4,R5]) = {(x10704: R5[Value]) =>
                          val o5 = implicitly[Ops[R5]]
                          implicit def convert_ev4_5(x: R4[Value]): R5[Value] = ev4_5.convert(x)
                          val ev3_5: Convert[R3,R5] = convertTrans[R3,R4,R5](ev3_4, ev4_5); implicit def convert_ev3_5(x: R3[Value]): R5[Value] = ev3_5.convert(x); val ev2_5: Convert[R2,R5] = convertTrans[R2,R3,R5](ev2_3, ev3_5); implicit def convert_ev2_5(x: R2[Value]): R5[Value] = ev2_5.convert(x); val ev1_5: Convert[R1,R5] = convertTrans[R1,R2,R5](ev1_2, ev2_5); implicit def convert_ev1_5(x: R1[Value]): R5[Value] = ev1_5.convert(x); val ev_5: Convert[R,R5] = convertTrans[R,R1,R5](ev_1, ev1_5); implicit def convert_ev_5(x: R[Value]): R5[Value] = ev_5.convert(x);
                          val x10783 = o5._cell_read(o5._lift(Cell("fib")))
                          val x10838 = o5._cell_read(o5._lift(Cell("counter")))
                          val x10839 = o5._cons(x10838, o5._lift(P(I(1), N)))
                          val x10857 = o5._app(o5._lift(Prim("+")), x10839,
                          o5._cont(new FunC[R5] { def fun[R6[_]:Ops](implicit ev5_6: Convert[R5,R6]) = {(x10840: R6[Value]) =>
                              val o6 = implicitly[Ops[R6]]
                              implicit def convert_ev5_6(x: R5[Value]): R6[Value] = ev5_6.convert(x)
                              val ev4_6: Convert[R4,R6] = convertTrans[R4,R5,R6](ev4_5, ev5_6); implicit def convert_ev4_6(x: R4[Value]): R6[Value] = ev4_6.convert(x); val ev3_6: Convert[R3,R6] = convertTrans[R3,R4,R6](ev3_4, ev4_6); implicit def convert_ev3_6(x: R3[Value]): R6[Value] = ev3_6.convert(x); val ev2_6: Convert[R2,R6] = convertTrans[R2,R3,R6](ev2_3, ev3_6); implicit def convert_ev2_6(x: R2[Value]): R6[Value] = ev2_6.convert(x); val ev1_6: Convert[R1,R6] = convertTrans[R1,R2,R6](ev1_2, ev2_6); implicit def convert_ev1_6(x: R1[Value]): R6[Value] = ev1_6.convert(x); val ev_6: Convert[R,R6] = convertTrans[R,R1,R6](ev_1, ev1_6); implicit def convert_ev_6(x: R[Value]): R6[Value] = ev_6.convert(x);
                              val x10841 = o6._cell_set(o6._lift(Cell("counter")), x10840)
                              val x10843 = x1
                              val x10844 = o6._cons(x10843, o6._lift(P(I(2), N)))
                              val x10855 = o6._app(o6._lift(Prim("-")), x10844,
                              o6._cont(new FunC[R6] { def fun[R7[_]:Ops](implicit ev6_7: Convert[R6,R7]) = {(x10845: R7[Value]) =>
                                  val o7 = implicitly[Ops[R7]]
                                  implicit def convert_ev6_7(x: R6[Value]): R7[Value] = ev6_7.convert(x)
                                  val ev5_7: Convert[R5,R7] = convertTrans[R5,R6,R7](ev5_6, ev6_7); implicit def convert_ev5_7(x: R5[Value]): R7[Value] = ev5_7.convert(x); val ev4_7: Convert[R4,R7] = convertTrans[R4,R5,R7](ev4_5, ev5_7); implicit def convert_ev4_7(x: R4[Value]): R7[Value] = ev4_7.convert(x); val ev3_7: Convert[R3,R7] = convertTrans[R3,R4,R7](ev3_4, ev4_7); implicit def convert_ev3_7(x: R3[Value]): R7[Value] = ev3_7.convert(x); val ev2_7: Convert[R2,R7] = convertTrans[R2,R3,R7](ev2_3, ev3_7); implicit def convert_ev2_7(x: R2[Value]): R7[Value] = ev2_7.convert(x); val ev1_7: Convert[R1,R7] = convertTrans[R1,R2,R7](ev1_2, ev2_7); implicit def convert_ev1_7(x: R1[Value]): R7[Value] = ev1_7.convert(x); val ev_7: Convert[R,R7] = convertTrans[R,R1,R7](ev_1, ev1_7); implicit def convert_ev_7(x: R[Value]): R7[Value] = ev_7.convert(x);
                                  val x10846 = o7._cons(x10845, o7._lift(N))
                                  val x10853 = o7._app(x10783, x10846,
                                  o7._cont(new FunC[R7] { def fun[R8[_]:Ops](implicit ev7_8: Convert[R7,R8]) = {(x10847: R8[Value]) =>
                                      val o8 = implicitly[Ops[R8]]
                                      implicit def convert_ev7_8(x: R7[Value]): R8[Value] = ev7_8.convert(x)
                                      val ev6_8: Convert[R6,R8] = convertTrans[R6,R7,R8](ev6_7, ev7_8); implicit def convert_ev6_8(x: R6[Value]): R8[Value] = ev6_8.convert(x); val ev5_8: Convert[R5,R8] = convertTrans[R5,R6,R8](ev5_6, ev6_8); implicit def convert_ev5_8(x: R5[Value]): R8[Value] = ev5_8.convert(x); val ev4_8: Convert[R4,R8] = convertTrans[R4,R5,R8](ev4_5, ev5_8); implicit def convert_ev4_8(x: R4[Value]): R8[Value] = ev4_8.convert(x); val ev3_8: Convert[R3,R8] = convertTrans[R3,R4,R8](ev3_4, ev4_8); implicit def convert_ev3_8(x: R3[Value]): R8[Value] = ev3_8.convert(x); val ev2_8: Convert[R2,R8] = convertTrans[R2,R3,R8](ev2_3, ev3_8); implicit def convert_ev2_8(x: R2[Value]): R8[Value] = ev2_8.convert(x); val ev1_8: Convert[R1,R8] = convertTrans[R1,R2,R8](ev1_2, ev2_8); implicit def convert_ev1_8(x: R1[Value]): R8[Value] = ev1_8.convert(x); val ev_8: Convert[R,R8] = convertTrans[R,R1,R8](ev_1, ev1_8); implicit def convert_ev_8(x: R[Value]): R8[Value] = ev_8.convert(x);
                                      val x10848 = o8._cons(x10847, o8._lift(N))
                                      val x10849 = o8._cons(x10704, x10848)
                                      val x10851 = o8._app(o8._lift(Prim("+")), x10849,
                                      o8._cont(new FunC[R8] { def fun[R9[_]:Ops](implicit ev8_9: Convert[R8,R9]) = {(x10850: R9[Value]) =>
                                          x10850: R9[Value]
                                      }})
                                      )
                                      x10851: R8[Value]
                                  }})
                                  )
                                  x10853: R7[Value]
                              }})
                              )
                              x10855: R6[Value]
                          }})
                          )
                          x10857: R5[Value]
                      }})
                      )
                      x10859: R4[Value]
                  }})
                  )
                  x10861: R3[Value]
              }})
              )
              x10863
            })(o2.valueTag)
            x10865: R2[Value]
        }})
        )
        x10867: R1[Value]
    }})
    )
    x10869
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
