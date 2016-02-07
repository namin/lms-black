package scala.lms.black

import eval._
import parser._

object repl {
  def id_cont = OpsNoRep._cont(new FunC[NoRep]{def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = {x => x}})
  var global_env = init_env
  var global_menv = init_menv[NoRep]
  def ev(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    meta_apply[NoRep](global_menv, S("base-eval"), e, global_env, id_cont)
  }
  def clean() = {
    reset()
    global_env = init_env
    global_menv = init_menv[NoRep]
  }
}
