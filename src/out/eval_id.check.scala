/*****************************************
Emitting Generated Code
*******************************************/
import language.higherKinds
import scala.lms.black.eval._
class Snippet extends Fun[NoRep] with (((Value, Cont[NoRep])) => Value) {
  def apply(v: (Value, Cont[NoRep])): Value = v._2(v._1)
  def fun[R[_]:Ops] = { v => fun[R]((v._1, v._2))  }
  def fun[R[_]:Ops](x0:(Value, Cont[R])): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    val x1 = x0._1
    val x2 = x0._2
    val x3 = x2(x1)
    x3
  }
}
/*****************************************
End of Generated Code
*******************************************/
// compilation: ok
