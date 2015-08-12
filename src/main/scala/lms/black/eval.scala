package scala.lms.black

object eval {
  type Env = Value
  type Cont[R[_]] = R[Value] => R[Value]
  sealed trait Value
  case class I(n: Int) extends Value
  case class B(b: Boolean) extends Value
  case class S(sym: String) extends Value {
    override def toString = "S(\""+sym+"\")"
  }
  case object N extends Value
  case class P(car: Value, cdr: Value) extends Value
  case class Prim(p: String) extends Value {
    override def toString = "Prim(\""+p+"\")"
  }
  case class Clo(param: Value, body: Value, env: Env) extends Value
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

  def cons(car: Value, cdr: Value) = P(car, cdr)
  def car(v: Value) = v match {
    case P(a, d) => a
  }
  def cdr(v: Value) = v match {
    case P(a, d) => d
  }
  def apply_primitive(p: String, args: Value): Value = (p, args) match {
    case ("<", P(I(a), P(I(b), N))) => B(a < b)
    case ("+", P(I(a), P(I(b), N))) => I(a+b)
    case ("-", P(I(a), P(I(b), N))) => I(a-b)
    case ("car", P(v, N)) => car(v)
    case ("cdr", P(v, N)) => cdr(v)
    case ("cons", P(a, P(d, N))) => cons(a, d)
  }

  trait Ops[R[_]] {
    implicit def lift(v: Value): R[Value]
    def app(fun: R[Value], args: R[Value], env: Env, cont: Cont[R]): R[Value]
    def isTrue(v: R[Value]): R[Boolean]
    def ifThenElse[A:Manifest](cond: R[Boolean], thenp: => R[A], elsep: => R[A]): R[A]
    def makeFun(f: Fun[R]): R[Value]
    def makePair(car: R[Value], cdr: R[Value]): R[Value]
    def inRep: Boolean
  }

  type NoRep[A] = A
  implicit object OpsNoRep extends Ops[NoRep] {
    def lift(v: Value) = v
    def app(fun: Value, args: Value, env: Env, cont: Cont[NoRep]) =
      fun match {
        case Clo(param, body, cenv) =>
          base_eval[NoRep](body, env_add(cenv, param, car(args)), cont)
        case Evalfun(key) =>
          val f = funs(key).fun[NoRep]
          f(car(args), cont)
        case Prim(p) =>
          cont(apply_primitive(p, args))
      }
    def isTrue(v: Value) = B(false)!=v
    def ifThenElse[A:Manifest](cond: Boolean, thenp: => A, elsep: => A): A = if (cond) thenp else elsep
    def makeFun(f: Fun[NoRep]) = evalfun(f)
    def makePair(car: Value, cdr: Value) = cons(car, cdr)
    def inRep = false
  }

  def base_apply[R[_]:Ops](fun: R[Value], args: R[Value], env: Env, cont: Cont[R]) = {
    val o = implicitly[Ops[R]]
    o.app(fun, args, env, cont)
  }

  def base_evlist[R[_]:Ops](exps: Value, env: Env, cont: Cont[R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    exps match {
      case N => cont(lift(N))
      case P(e, es) => base_eval[R](e, env, { v =>
        base_evlist[R](es, env, { vs =>
          cont(makePair(v, vs))
        })
      })
    }
  }

  def base_eval_fun: Fun[NoRep] = new Fun[NoRep] {
    def fun[R[_]:Ops] = { (vc: (Value, Cont[R])) =>
      val P(exp, env) = vc._1
      base_eval[R](exp, env, vc._2)
    }
  }
  def base_eval[R[_]:Ops](exp: Value, env: Env, cont: Cont[R]): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    exp match {
      case e@I(n) => cont(lift(e))
      case e@B(b) => cont(lift(e))
      case e@Prim(p) => cont(lift(e))
      case e@S(sym) => env_get(env, e) match {
        case Code(v) => cont(v.asInstanceOf[R[Value]])
        case v => cont(lift(v))
      }
      case P(S("lambda"), _) =>
        val (param, body) = exp match {
          case P(_, P(P(param, N), P(body, N))) => (param, body)
        }
        cont(lift(Clo(param, body, env)))
      case P(S("clambda"), _) =>
        val (param, body) = exp match {
          case P(_, P(P(param, N), P(body, N))) => (param, body)
        }
        if (!inRep) {
          trait Program extends EvalDsl {
            def snippet(v: Rep[(Value, Value => Value)]): Rep[Value] = {
              base_eval[Rep](body, env_add(env, param, Code(v._1)), x => (v._2(x)))(OpsRep)
            }
          }
          val r = new EvalDslDriver with Program
          println(r.code)
          r.precompile
          cont(lift(evalfun(r.f)))
        } else {
          val f = makeFun(new Fun[R] {
            def fun[RF[_]:Ops] = {(v: (R[Value], Cont[RF])) =>
              base_eval[RF](body, env_add(env, param, Code(v._1)), v._2)
            }
          })
          cont(f)
        }
      case P(S("if"), _) =>
        val (cond, thenp, elsep) = exp match {
          case P(_, P(cond, P(thenp, P(elsep, N)))) => (cond, thenp, elsep)
        }
        base_eval[R](cond, env, { vc =>
          ifThenElse(isTrue(vc),
            base_eval[R](thenp, env, cont),
            base_eval[R](elsep, env, cont))
        })
      case P(k@S("hack"), _) =>
        env_get(env, k) match {
          case Evalfun(key) =>
            val f = funs(key).fun[R]
            f(P(exp, env), cont)
        }
      case P(fun, args) => base_eval[R](fun, env, { v =>
        base_evlist[R](args, env, { vs =>
          base_apply[R](v, vs, env, cont)
        })
      })
    }
  }

  def env_add(env: Value, k: Value, v: Value) = P(P(k, v), env)
  def env_get(env: Value, key: Value): Value = env match {
    case P(P(k, v), r) => if (k==key) v else env_get(r, key)
    case _ => throw new Error("unbound variable "+key+" in "+env)
  }
  def init_env = P(P(S("base_eval"), evalfun(base_eval_fun)), N)

  def top_eval[R[_]:Ops](exp: Value): R[Value] = {
    reset()
    base_eval[R](exp, init_env, x => x)
  }
}

import eval._
import scala.lms.common._

trait EvalDsl extends Functions with TupleOps with IfThenElse with Equal with UncheckedOps {
  def base_apply_rep(f: Rep[Value], args: Rep[Value], env: Env, cont: Cont[Rep]): Rep[Value]
  implicit object OpsRep extends scala.Serializable with Ops[Rep] {
    def lift(v: Value) = unit(v)
    def app(f: Rep[Value], args: Rep[Value], env: Env, cont: Cont[Rep]) =
      base_apply_rep(f, args, env, cont)
    def isTrue(v: Rep[Value]): Rep[Boolean] = unit(B(false))!=v
    def ifThenElse[A:Manifest](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A] = if (cond) thenp else elsep
    def makeFun(f: Fun[Rep]) = {
      val fn = f.fun[Rep]
      unchecked("evalfun(new Fun[NoRep] { def fun[R[_]:Ops] = ",
        fun{(v: Rep[(Value, Value => Value)]) => fn(v._1, x => v._2(x))},".asInstanceOf[((Value, Cont[R])) => R[Value]]",
      "})")
    }
    def makePair(car: Rep[Value], cdr: Rep[Value]) =
      unchecked("makePair(", car, ", ", cdr, ")")
    def inRep = true
  }

  def snippet(v: Rep[(Value, Value => Value)]): Rep[Value]
}

trait EvalDslExp extends EvalDsl with EffectExp with FunctionsRecursiveExp with TupleOpsExp with IfThenElseExp with EqualExp with UncheckedOpsExp {
  case class BaseApplyRep(f: Rep[Value], args: Rep[Value], env: Env, cont: Rep[Cont[NoRep]]) extends Def[Value]
  def base_apply_rep(f: Rep[Value], args: Rep[Value], env: Env, cont: Cont[Rep]): Rep[Value] =
    reflectEffect(BaseApplyRep(f, args, env, fun(cont)))
}

trait EvalDslGen extends ScalaGenFunctions with ScalaGenTupleOps with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenUncheckedOps {
  val IR: EvalDslExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case Const(P(a, d)) => "P("+quote(Const(a))+", "+quote(Const(d))+")"
    case Const(Code(c)) => quote(c.asInstanceOf[Rep[Any]])
    case Const(Clo(param, body, env)) =>  "Clo(\""+quote(Const(param))+"\", "+quote(Const(body))+", "+quote(Const(env))+")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BaseApplyRep(f, args, env, cont) =>
      emitValDef(sym, "base_apply[R]("+quote(f)+", "+quote(args)+", "+quote(Const(env))+", "+quote(cont)+")")
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
