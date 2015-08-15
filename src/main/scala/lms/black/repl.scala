package scala.lms.black

import eval._
import parser._

object repl {
  def idcont = mkCont[NoRep]{x => x}
  var global_env = init_env
  def ev(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    base_eval[NoRep](init_mcont, e, global_env, idcont)
  }
  def clean() = {
    reset()
    global_env = init_env
  }
}
