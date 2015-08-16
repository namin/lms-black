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
    val x4 = o.cellRead(x2)
    x4
  }
}
/*****************************************
End of Generated Code
*******************************************/
compilation: ok
