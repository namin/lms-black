package scala.lms.black

object eval {
  sealed trait Term
  type Env = Map[String, Value]
  type Cont[R[_]] = R[Value] => R[Value]
  sealed trait Value
  case class I(n: Int) extends Value with Term
  case class B(b: Boolean) extends Value with Term
  case class V(s: String) extends Term {
    override def toString = "V(\""+s+"\")"
  }
  case class P(p: String) extends Value with Term {
    override def toString = "P(\""+p+"\")"
  }
  case class L(compile: Boolean, param: String, body: Term) extends Term {
    override def toString = "L("+compile+", \""+param+"\", "+body+")"
  }
  case class A(fun: Term, args: List[Term]) extends Term
  case class If(c: Term, thenp: Term, elsep: Term) extends Term
  case class Clo(param: String, body: Term, env: Map[String, Value]) extends Value
  case class Evalfun(key: String) extends Value {
    override def toString = "Evalfun(\""+key+"\")"
  }
  case class Code[R[_]](c: R[Value]) extends Value

  abstract class Fun[W[_]:Ops] extends Serializable {
    def fun[R[_]:Ops]: ((W[Value], Cont[R])) => R[Value]
  }
  var funs = Map[String, Fun[NoRep]]()
  def addFun(f: Fun[NoRep]): String = {
    val key = "f"+funs.size
    funs += (key -> f)
    key
  }
  def reset() {
    funs = funs.empty
  }
  def evalfun(f: Fun[NoRep]) = Evalfun(addFun(f))

  def apply_primitive(p: String, args: List[Value]): Value = (p, args) match {
    case ("<", List(I(a), I(b))) => B(a < b)
    case ("+", List(I(a), I(b))) => I(a+b)
    case ("-", List(I(a), I(b))) => I(a-b)
  }

  trait Ops[R[_]] {
    implicit def lift(v: Value): R[Value]
    def app(fun: R[Value], args: List[R[Value]], env: Env, cont: Cont[R]): R[Value]
    def isTrue(v: R[Value]): R[Boolean]
    def ifThenElse[A:Manifest](cond: R[Boolean], thenp: => R[A], elsep: => R[A]): R[A]
    def makeFun(f: Fun[R]): R[Value]
    def inRep: Boolean
  }

  type NoRep[A] = A
  implicit object OpsNoRep extends Ops[NoRep] {
    def lift(v: Value) = v
    def app(fun: Value, args: List[Value], env: Env, cont: Cont[NoRep]) =
      fun match {
        case Clo(param, body, cenv) =>
          base_eval[NoRep](body, cenv + (param -> args(0)), cont)
        case Evalfun(key) =>
          val f = funs(key).fun[NoRep]
          f(args(0), cont)
        case P(p) =>
          cont(apply_primitive(p, args))
      }
    def isTrue(v: Value) = B(false)!=v
    def ifThenElse[A:Manifest](cond: Boolean, thenp: => A, elsep: => A): A = if (cond) thenp else elsep
    def makeFun(f: Fun[NoRep]) = evalfun(f)
    def inRep = false
  }

  def base_apply[R[_]:Ops](fun: R[Value], args: List[R[Value]], env: Env, cont: Cont[R]) = {
    val o = implicitly[Ops[R]]
    o.app(fun, args, env, cont)
  }

  def base_evlist[R[_]:Ops](exps: List[Term], env: Env, cont: List[R[Value]] => R[Value]): R[Value] = exps match {
    case Nil => cont(Nil)
    case e::es => base_eval[R](e, env, { v =>
      base_evlist[R](es, env, { vs =>
        cont(v::vs)
      })
    })
  }

  def top_eval[R[_]:Ops](exp: Term): R[Value] = {
    reset()
    base_eval[R](exp, Map(), x => x)
  }

  def base_eval[R[_]:Ops](exp: Term, env: Env, cont: Cont[R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    exp match {
      case e@I(n) => cont(lift(e))
      case e@B(b) => cont(lift(e))
      case e@P(p) => cont(lift(e))
      case V(s) => env.get(s) match {
        case Some(Code(v)) => cont(v.asInstanceOf[R[Value]])
        case Some(v) => cont(lift(v))
      }
      case L(compile, param, body) =>
        if (!compile) {
          cont(lift(Clo(param, body, env)))
        } else if (!inRep) {
          trait Program extends EvalDsl {
            def snippet(v: Rep[(Value, Value => Value)]): Rep[Value] = {
              base_eval[Rep](body, env+(param -> Code(v._1)), x => (v._2(x)))(OpsRep)
            }
          }
          val r = new EvalDslDriver with Program
          println(r.code)
          r.precompile
          cont(lift(evalfun(r.f)))
        } else {
          val f = makeFun(new Fun[R] {
            def fun[RF[_]:Ops] = (v: (R[Value], Cont[RF])) => base_eval[RF](body, env+(param -> Code(v._1)), v._2)
          })
          cont(f)
        }
      case A(fun, args) => base_eval[R](fun, env, { v =>
        base_evlist[R](args, env, { vs =>
          base_apply[R](v, vs, env, cont)
        })
      })
      case If(cond, thenp, elsep) => base_eval[R](cond, env, { vc =>
        ifThenElse(isTrue(vc),
          base_eval[R](thenp, env, cont),
          base_eval[R](elsep, env, cont))
      })
    }
  }
}

import eval._
import scala.lms.common._

trait EvalDsl extends Functions with TupleOps with IfThenElse with Equal with UncheckedOps {
  def base_apply_rep(f: Rep[Value], args: List[Rep[Value]], env: Env, cont: Cont[Rep]): Rep[Value]
  implicit object OpsRep extends scala.Serializable with Ops[Rep] {
    def lift(v: Value) = unit(v)
    def app(f: Rep[Value], args: List[Rep[Value]], env: Env, cont: Cont[Rep]) =
      base_apply_rep(f, args, env, cont)
    def isTrue(v: Rep[Value]): Rep[Boolean] = unit(B(false))!=v
    def ifThenElse[A:Manifest](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A] = if (cond) thenp else elsep
    def makeFun(f: Fun[Rep]) = {
      val fn = f.fun[Rep]
      unchecked("evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = ",
        fun{(v: Rep[(Value, Value => Value)]) => fn(v._1, x => v._2(x))},".asInstanceOf[((Value, Cont[R])) => R[Value]]",
      "})")
    }
    def inRep = true
  }

  def snippet(v: Rep[(Value, Value => Value)]): Rep[Value]
}

trait EvalDslExp extends EvalDsl with EffectExp with FunctionsRecursiveExp with TupleOpsExp with IfThenElseExp with EqualExp with UncheckedOpsExp {
  case class BaseApplyRep(f: Rep[Value], args: List[Rep[Value]], env: Env, cont: Rep[Cont[NoRep]]) extends Def[Value]
  def base_apply_rep(f: Rep[Value], args: List[Rep[Value]], env: Env, cont: Cont[Rep]): Rep[Value] =
    reflectEffect(BaseApplyRep(f, args, env, fun(cont)))
}

trait EvalDslGen extends ScalaGenFunctions with ScalaGenTupleOps with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenUncheckedOps {
  val IR: EvalDslExp
  import IR._

  def env_quote(env: Env) =
    "Map("+(for ((k,v) <- env) yield ("(\""+k+"\" -> "+quote(Const(v))+")")).mkString(", ")+")"

  override def quote(x: Exp[Any]) : String = x match {
    case Const(Code(c)) => "Code[R]("+quote(c.asInstanceOf[Rep[Any]])+")"
    case Const(Clo(param, body, env)) =>  "Clo(\""+param+"\", "+body+", "+env_quote(env)+")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BaseApplyRep(f, args, env, cont) =>
      emitValDef(sym, "base_apply[R]("+quote(f)+", List("+args.map(quote).mkString(", ")+"), "+env_quote(env)+", "+quote(cont)+")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait EvalDslImpl extends EvalDslExp { q =>
  val codegen = new EvalDslGen {
    val IR: q.type = q

    override def remap[A](m: Manifest[A]): String = {
      val s = m.toString
      if (s=="scala.Tuple2[scala.lms.black.eval$Value, scala.Function1[scala.lms.black.eval$Value, scala.lms.black.eval$Value]]") "(Value, Cont[R])"
      else if (s=="scala.Function1[scala.lms.black.eval$Value, scala.lms.black.eval$Value]") "Cont[R]"
      else if (s=="scala.lms.black.eval$Value") "R[Value]"
      else super.remap(m)
    }

    override def emitFileHeader() {
      stream.println("import language.higherKinds")
      stream.println("import scala.lms.black.eval._")
    }

    override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: java.io.PrintWriter) = {
      val sA = remap(manifest[A])

      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println("/*****************************************\n"+
                       "  Emitting Generated Code                  \n"+
                       "*******************************************/")
        emitFileHeader()

        stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends Fun[NoRep] with (((Value, Cont[NoRep])) => Value) {")

        stream.println("def apply(v: (Value, Cont[NoRep])): Value = v._2(v._1)")
        stream.println("def fun[R[_]:Ops] = { v => fun[R]((v._1, v._2))  }")
        stream.println("def fun[R[_]:Ops]("+args.map(a => quote(a) + ":" + remap(a.tp)).mkString(", ")+"): "+sA+" = {")
        stream.println("val o = implicitly[Ops[R]]; import o._")

        emitBlock(body)
        stream.println(quote(getBlockResult(body)))

        stream.println("}")

        stream.println("}")
        stream.println("/*****************************************\n"+
                       "  End of Generated Code                  \n"+
                       "*******************************************/")
      }
    staticData
    }
  }
}

abstract class EvalDslDriver extends EvalDsl with EvalDslImpl with CompileScala {
  lazy val f = compile(snippet).asInstanceOf[Fun[NoRep]]
  def precompile: Unit = { print("// "); f }
  def precompileSilently: Unit = utils.devnull(f)
  def eval[R[_]:Ops](v: Value, cont: Cont[R]): R[Value] = {
    val fn = f.fun[R]
    fn(v, cont)
  }
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
  }
}
