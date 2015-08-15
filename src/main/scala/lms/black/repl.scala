package scala.lms.black

import eval._
import parser._

object repl {
  def id_cont = mkCont[NoRep]{x => x}
  var global_env = init_env
  var global_menv = init_menv[NoRep]
  def ev(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    base_eval[NoRep](global_menv, e, global_env, id_cont)
  }
  def clean() = {
    reset()
    global_env = init_env
    global_menv = init_menv[NoRep]
  }
}
