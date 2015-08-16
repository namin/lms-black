/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class staged$0 extends Fun[NoRep] with (Value => Value) {
  def apply(v: Value): Value = v
  def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { (m: MEnv) => { v => fun[R](m, v)(implicitly[Ops[R]], ev)  } }
  def fun[R[_]:Ops](m: MEnv, x0:Value)(implicit ev: Convert[NoRep,R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = o.getCar(x0)
    val x2 = Code(o.cellNew(x1))
    val x3 = o.getCdr(x0)
    val x12 = {x11: (R[Value]) =>
      x11: R[Value]
    }
    val x15 = o.makeFun(m, new Fun[R] { def fun[R0[_]:Ops](implicit ev: Convert[R,R0]) = { (m: MEnv) => {(x4: R[Value]) =>
          val o = implicitly[Ops[R0]]; import o._
          import ev._
          val x5 = o.getCar(x4)
          val x6 = Code(o.cellNew(x5))
          val x7 = o.getCdr(x4)
          val x8 = o.cellRead(x6)
          val x9 = o.cellRead(x6)
          val x10 = o.makePair(x9, N)
          val x13 = o.app(MEnv(9), x8, x10, P(P(P(S("F"), x6), N), P(P(P(S("fun"), x2), N), P(Cell(21), N))), mkCont[R](x12))
          x13: R0[Value]
    }}})
    val x29 = {x28: (R[Value]) =>
      x28: R[Value]
    }
    val x34 = {x33: (R[Value]) =>
      x33: R[Value]
    }
    val x40 = {x39: (R[Value]) =>
      x39: R[Value]
    }
    val x43 = o.makeFun(m, new Fun[R] { def fun[R0[_]:Ops](implicit ev: Convert[R,R0]) = { (m: MEnv) => {(x16: R[Value]) =>
          val o = implicitly[Ops[R0]]; import o._
          import ev._
          val x17 = o.getCar(x16)
          val x18 = Code(o.cellNew(x17))
          val x19 = o.getCdr(x16)
          val x20 = o.cellRead(x2)
          val x37 = o.makeFun(m, new Fun[R0] { def fun[R1[_]:Ops](implicit ev: Convert[R0,R1]) = { (m: MEnv) => {(x21: R0[Value]) =>
                val o = implicitly[Ops[R1]]; import o._
                import ev._
                val x22 = o.getCar(x21)
                val x23 = Code(o.cellNew(x22))
                val x24 = o.getCdr(x21)
                val x25 = o.cellRead(x18)
                val x26 = o.cellRead(x18)
                val x27 = o.makePair(x26, N)
                val x30 = o.app(MEnv(19), x25, x27, P(P(P(S("x"), x23), N), P(P(P(S("F"), x18), N), P(P(P(S("fun"), x2), N), P(Cell(21), N)))), mkCont[R](x29))
                val x31 = o.cellRead(x23)
                val x32 = o.makePair(x31, N)
                val x35 = o.app(MEnv(18), x30, x32, P(P(P(S("x"), x23), N), P(P(P(S("F"), x18), N), P(P(P(S("fun"), x2), N), P(Cell(21), N)))), mkCont[R](x34))
                x35: R1[Value]
          }}})
          val x38 = o.makePair(x37, N)
          val x41 = o.app(MEnv(14), x20, x38, P(P(P(S("F"), x18), N), P(P(P(S("fun"), x2), N), P(Cell(21), N))), mkCont[R](x40))
          x41: R0[Value]
    }}})
    val x44 = o.makePair(x43, N)
    val x46 = {x45: (R[Value]) =>
      x45: R[Value]
    }
    val x47 = o.app(MEnv(6), x15, x44, P(P(P(S("fun"), x2), N), P(Cell(21), N)), mkCont[R](x46))
    x47
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class staged$0 extends Fun[NoRep] with (Value => Value) {
  def apply(v: Value): Value = v
  def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { (m: MEnv) => { v => fun[R](m, v)(implicitly[Ops[R]], ev)  } }
  def fun[R[_]:Ops](m: MEnv, x0:Value)(implicit ev: Convert[NoRep,R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = o.getCar(x0)
    val x2 = Code(o.cellNew(x1))
    val x3 = o.getCdr(x0)
    val x12 = {x11: (R[Value]) =>
      x11: R[Value]
    }
    val x47 = o.makeFun(m, new Fun[R] { def fun[R0[_]:Ops](implicit ev: Convert[R,R0]) = { (m: MEnv) => {(x4: R[Value]) =>
          val o = implicitly[Ops[R0]]; import o._
          import ev._
          val x5 = o.getCar(x4)
          val x6 = Code(o.cellNew(x5))
          val x7 = o.getCdr(x4)
          val x8 = o.cellRead(x6)
          val x9 = o.makePair(I(2), N)
          val x10 = o.makePair(x8, x9)
          val x13 = o.app(MEnv(28), Prim("<"), x10, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x12))
          val x14 = o.isTrue(x13)
          val x45 = o.ifThenElse((x14), {
            val x15 = o.cellRead(x6)
            x15
          }, {
            val x17 = o.cellRead(x2)
            val x18 = o.cellRead(x6)
            val x19 = o.makePair(I(1), N)
            val x20 = o.makePair(x18, x19)
            val x22 = {x21: (R[Value]) =>
              x21: R[Value]
            }
            val x23 = o.app(MEnv(36), Prim("-"), x20, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x22))
            val x24 = o.makePair(x23, N)
            val x26 = {x25: (R[Value]) =>
              x25: R[Value]
            }
            val x27 = o.app(MEnv(34), x17, x24, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x26))
            val x28 = o.cellRead(x2)
            val x29 = o.cellRead(x6)
            val x30 = o.makePair(I(2), N)
            val x31 = o.makePair(x29, x30)
            val x33 = {x32: (R[Value]) =>
              x32: R[Value]
            }
            val x34 = o.app(MEnv(41), Prim("-"), x31, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x33))
            val x35 = o.makePair(x34, N)
            val x37 = {x36: (R[Value]) =>
              x36: R[Value]
            }
            val x38 = o.app(MEnv(39), x28, x35, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x37))
            val x39 = o.makePair(x38, N)
            val x40 = o.makePair(x27, x39)
            val x42 = {x41: (R[Value]) =>
              x41: R[Value]
            }
            val x43 = o.app(MEnv(32), Prim("+"), x40, P(P(P(S("n"), x6), N), P(P(P(S("fib"), x2), N), P(Cell(21), N))), mkCont[R](x42))
            x43
          })
          x45: R0[Value]
    }}})
    x47
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
