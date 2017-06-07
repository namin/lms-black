package scala.lms.black

import eval._
import parser._

object repl {
  var global_env = init_env
  var global_menv = init_menv[NoRep]
  def parse(s: String) = {
    val Success(e, _) = parseAll(exp, s)
    e
  }
  def evl(e: Value) = {
    meta_apply[NoRep](global_menv, S("base-eval"), e, global_env, id_cont[NoRep])
  }
  def ev(s: String) = evl(parse(s))
  def clean() = {
    reset()
    global_env = init_env
    global_menv = init_menv[NoRep]
  }
}
