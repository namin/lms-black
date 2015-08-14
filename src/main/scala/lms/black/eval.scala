package scala.lms.black

object eval {
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
  case class Clo(params: Value, body: Value, env: Value) extends Value
  case class Evalfun(key: Int) extends Value
  case class Code[R[_]](c: R[Value]) extends Value
  case class Cont(key: Int) extends Value
  case class Cell(key: Int) extends Value

  var cells = Map[Int, Value]()
  def addCell(v: Value): Int = {
    val key = cells.size
    cells += (key -> v)
    key
  }
  def cell_new(v: Value) = Cell(addCell(v))
  def cell_read(c: Value): Value = c match {
    case Cell(key) => cells(key)
    case Code(cc) => cell_read(cc.asInstanceOf[Value])
  }
  def cell_set(c: Value, v: Value): Value = c match {
    case Cell(key) => cells += (key -> v); v
    case Code(cc) => cell_set(cc.asInstanceOf[Value], v)
  }
  abstract class Fun[W[_]:Ops] extends Serializable {
    def fun[R[_]:Ops]: W[Value] => R[Value]
  }
  var funs = Map[Int, Fun[NoRep]]()
  def addFun(f: Fun[NoRep]): Int = {
    val key = funs.size
    funs += (key -> f)
    key
  }
  abstract class FunC extends Serializable {
    def fun[R[_]:Ops]: R[Value] => R[Value]
  }
  var conts = Map[Int, FunC]()
  def addCont(f: FunC): Int = {
    val key = conts.size
    conts += (key -> f)
    key
  }
  def reset() {
    funs = funs.empty
    conts = conts.empty
    cells = cells.empty
  }
  def evalfun(f: Fun[NoRep]) = Evalfun(addFun(f))
  def mkCont[R[_]:Ops](f: R[Value] => R[Value]): Value = Cont(addCont(new FunC {
    def fun[R[_]:Ops] = f.asInstanceOf[R[Value] => R[Value]]
  }))

  def cons(car: Value, cdr: Value) = P(car, cdr)
  def car(v: Value) = v match {
    case P(a, d) => a
  }
  def cdr(v: Value) = v match {
    case P(a, d) => d
  }

  def apply_cont[R[_]:Ops](cont: Value, v: R[Value]): R[Value] = cont match {
    case Cont(key) =>
      val f = conts(key).fun[R]
      f(v)
  }

  trait Ops[R[_]] {
    implicit def lift(v: Value): R[Value]
    def app(fun: R[Value], args: R[Value], env: Value, cont: Value): R[Value]
    def isTrue(v: R[Value]): R[Boolean]
    def ifThenElse[A:Manifest](cond: R[Boolean], thenp: => R[A], elsep: => R[A]): R[A]
    def makeFun(f: Fun[R]): R[Value]
    def makePair(car: R[Value], cdr: R[Value]): R[Value]
    def getCar(p: R[Value]): R[Value]
    def getCdr(p: R[Value]): R[Value]
    def cellNew(v: R[Value]): R[Value]
    def cellRead(c: R[Value]): R[Value]
    def cellSet(c: R[Value], v: R[Value]): R[Value]
    def inRep: Boolean
  }
  def static_apply[R[_]:Ops](fun: Value, args: Value, env: Value, cont: Value) = {
    val o = implicitly[Ops[R]]; import o._
    fun match {
      case Clo(params, body, cenv) =>
        base_eval[R](body, env_extend[R](cenv, params, args), cont)
      case Evalfun(key) =>
        val f = funs(key).fun[R]
        apply_cont[R](cont, f(args))
      case Prim(p) =>
        apply_cont[R](cont, apply_primitive(p, args))
    }
  }
  type NoRep[A] = A
  implicit object OpsNoRep extends Ops[NoRep] {
    def lift(v: Value) = v
    def app(fun: Value, args: Value, env: Value, cont: Value) =
      static_apply[NoRep](fun, args, env, cont)
    def isTrue(v: Value) = v match {
      case B(b) => b
    }
    def ifThenElse[A:Manifest](cond: Boolean, thenp: => A, elsep: => A): A = if (cond) thenp else elsep
    def makeFun(f: Fun[NoRep]) = evalfun(f)
    def makePair(car: Value, cdr: Value) = cons(car, cdr)
    def getCar(p: Value) = car(p)
    def getCdr(p: Value) = cdr(p)
    def cellNew(v: Value) = cell_new(v)
    def cellRead(c: Value) = cell_read(c)
    def cellSet(c: Value, v: Value) = cell_set(c, v)
    def inRep = false
  }

  def base_apply[R[_]:Ops](fun: R[Value], args: R[Value], env: Value, cont: Value) = {
    val o = implicitly[Ops[R]]
    o.app(fun, args, env, cont)
  }

  def base_evlist[R[_]:Ops](exps: Value, env: Value, cont: Value): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    exps match {
      case N => apply_cont[R](cont, lift(N))
      case P(e, es) => base_eval[R](e, env, mkCont[R]({ v =>
        base_evlist[R](es, env, mkCont[R]({ vs =>
          apply_cont[R](cont, makePair(v, vs))
        }))
      }))
    }
  }

  def base_eval_fun: Fun[NoRep] = new Fun[NoRep] {
    def fun[R[_]:Ops] = { (vc: Value) =>
      val P(exp, P(env, P(cont, N))) = vc
      base_eval[R](exp, env, cont)
    }
  }
  def base_eval[R[_]:Ops](exp: Value, env: Value, cont: Value): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    exp match {
      case I(_) | B(_) => apply_cont[R](cont, lift(exp))
      case S(sym) => meta_eval_var[R](exp, env, cont)
      case P(S("lambda"), _) =>
        val (params, body) = exp match {
          case P(_, P(params, P(body, N))) => (params, body)
        }
        apply_cont(cont, lift(Clo(params, body, env)))
      case P(S("clambda"), _) =>
        val (params, body) = exp match {
          case P(_, P(params, P(body, N))) => (params, body)
        }
        if (!inRep) {
          trait Program extends EvalDsl {
            def snippet(v: Rep[Value]): Rep[Value] = {
              base_eval[Rep](body,
                env_extend[Rep](env, params, Code(v))(OpsRep),
                mkCont[Rep](x => x)(OpsRep))(OpsRep)
            }
          }
          val r = new EvalDslDriver with Program
          r.precompile
          apply_cont(cont, lift(evalfun(r.f)))
        } else {
          val f = makeFun(new Fun[R] {
            def fun[RF[_]:Ops] = {(v: R[Value]) =>
              base_eval[RF](body, env_extend[RF](env, params, Code(v)), mkCont[RF](x => x))
            }
          })
          apply_cont(cont, f)
        }
      case P(S("if"), _) =>
        val (cond, thenp, elsep) = exp match {
          case P(_, P(cond, P(thenp, P(elsep, N)))) => (cond, thenp, elsep)
        }
        base_eval[R](cond, env, mkCont[R]({ vc =>
          ifThenElse(isTrue(vc),
            base_eval[R](thenp, env, cont),
            base_eval[R](elsep, env, cont))
        }))
      case P(S("begin"), body) => eval_begin[R](body, env, cont)
      case P(S("set!"), _) =>
        val (name, body) = exp match {
          case P(_, P(name, P(body, N))) => (name, body)
        }
        base_eval[R](body, env, mkCont[R]({ v =>
          val p = env_get(env, name)
          cellSet(lift(p), v)
          apply_cont(cont, name)
        }))
      case P(S("define"), _) =>
        val (name, body) = exp match {
          case P(_, P(name, P(body, N))) => (name, body)
        }
        base_eval[R](body, env, mkCont[R]({ v =>
          val (c, frame) = env match {
            case P(c@Cell(key), _) => (c, cells(key))
          }
          cellSet(lift(c), makePair(makePair(name, cellNew(v)), frame))
          apply_cont(cont, name)
        }))
      case P(k@S("hack"), _) =>
        cell_read(env_get(env, k)) match {
          case Evalfun(key) =>
            val f = funs(key).fun[R]
            f(P(exp, P(env, P(cont, N))))
        }
      case P(fun, args) => base_eval[R](fun, env, mkCont[R]({ v =>
        base_evlist[R](args, env, mkCont[R]({ vs =>
          base_apply[R](v, vs, env, cont)
        }))
      }))
    }
  }

  def eval_begin[R[_]:Ops](body: Value, env: Value, cont: Value): R[Value] =
    body match {
      case P(exp, N) => base_eval[R](exp, env, cont)
      case P(exp, rest) => base_eval[R](exp, env, mkCont[R]{ _ =>
        eval_begin[R](rest, env, cont)
      })
    }

  def eval_var_fun: Fun[NoRep] = new Fun[NoRep] {
    def fun[R[_]:Ops] = { (vc: Value) =>
      val P(exp, P(env, P(cont, N))) = vc
      eval_var[R](exp, env, cont)
    }
  }
  def eval_var[R[_]:Ops](exp: Value, env: Value, cont: Value): R[Value] = {
    val o = implicitly[Ops[R]]; import o._
    env_get(env, exp) match {
      case Code(v: R[Value]) => apply_cont[R](cont, cellRead(v))
      case v@Cell(_) => apply_cont[R](cont, cellRead(v))
      case v => apply_cont(cont, lift(v))
    }
  }
  var inEvalVar = false
  def meta_eval_var[R[_]:Ops](exp: Value, env: Value, cont: Value): R[Value] = {
    if (inEvalVar) eval_var[R](exp, env, cont) else {
      inEvalVar = true
      val o = implicitly[Ops[R]]; import o._
      val fun = env_get(env, S("eval_var")) match {
        case v@Cell(_) => cell_read(v)
      }
      val k = mkCont[R]{v => inEvalVar = false; apply_cont(cont, v)}
      val kid = mkCont[R]{v => v}
      val (kargs, kout) = if (inRep) (kid, k) else (k, kid)
      val args = P(exp, P(env, P(kargs, N)))
      static_apply[R](fun, args, env, kout)
    }
  }

  def env_extend[R[_]:Ops](env: Value, params: Value, args: Value) =
    cons(make_pairs[R](params, args), env)
  def make_pairs[R[_]:Ops](ks: Value, vs: Value): Value = (ks, vs) match {
    case (N, N) => N
    case (N, Code(_)) => N
    //case (S(s), _) => cons(cons(ks, vs), N)
    case (P(k, ks), P(v, vs)) => cons(cons(k, cell_new(v)), make_pairs[R](ks, vs))
    case (P(k, ks), Code(c : R[Value])) =>
      val o = implicitly[Ops[R]]
      cons(cons(k, Code(o.cellNew(o.getCar(c)))), make_pairs[R](ks, Code(o.getCdr(c))))
  }
  def env_get_pair(env: Value, key: Value): Option[Value] = env match {
    case P(f, r) =>
      val frame = f match {
        case Cell(k) => cells(k)
        case f => f
      }
      frame_get(frame, key) match {
        case res@Some(p) => res
        case None => env_get_pair(r, key)
      }
    case _ => None
  }
  def env_get(env: Value, key: Value): Value = env_get_pair(env, key) match {
    case Some(P(_, v)) => v
    case _ => throw new Error("unbound variable "+key+" in "+env)
  }
  def frame_get(frame: Value, key: Value): Option[Value] = frame match {
    case P(p@P(k, v), r) => if (k==key) Some(p) else frame_get(r, key)
    case N => None
  }

  def apply_primitive(p: String, args: Value): Value = (p, args) match {
    case ("<", P(I(a), P(I(b), N))) => B(a < b)
    case ("+", P(I(a), P(I(b), N))) => I(a+b)
    case ("-", P(I(a), P(I(b), N))) => I(a-b)
    case ("car", P(v, N)) => car(v)
    case ("cdr", P(v, N)) => cdr(v)
    case ("cons", P(a, P(d, N))) => cons(a, d)
    case ("cell_new", P(v, N)) => cell_new(v)
    case ("cell_read", P(c, N)) => cell_read(c)
    case ("cell_set!", P(c, P(v, N))) => cell_set(c, v)
    case ("eq?", P(a, P(b, N))) => B(a==b)
    case ("display", P(a, N)) => display(a); I(0)
  }

  def init_frame = list_to_value(List(
    P(S("eval_var"), cell_new(evalfun(eval_var_fun))),
    P(S("base_eval"), evalfun(base_eval_fun)),
    P(S("<"), Prim("<")),
    P(S("+"), Prim("+")),
    P(S("-"), Prim("-")),
    P(S("car"), Prim("car")),
    P(S("cdr"), Prim("cdr")),
    P(S("cons"), Prim("cons")),
    P(S("cell_new"), Prim("cell_new")),
    P(S("cell_read"), Prim("cell_read")),
    P(S("cell_set!"), Prim("cell_set!")),
    P(S("eq?"), Prim("eq?")),
    P(S("display"), Prim("display"))
  ))
  def init_env = cons(Cell(addCell(init_frame)), N)

  def list_to_value(xs: List[Value]): Value = xs match {
    case Nil => N
    case (x::xs) => P(x, list_to_value(xs))
  }

  def addParen(p: (Boolean, String)) = {
    val (need_paren, s) = p
    if (need_paren) "("+s+")" else s
  }
  def pp(v: Value): (Boolean, String) = v match {
    case B(b) => (false, if (b) "#t" else "#f")
    case I(n) => (false, n.toString)
    case S(s) => (false, s)
    case N => (true, "")
    case P(a, N) => (true, addParen(pp(a)))
    case P(a, d) =>
      val s1 = addParen(pp(a))
      val (need_paren2, s2) = pp(d)
      if (need_paren2) (true, s1+" "+s2)
      else (true, s1+" . "+s2)
    case Prim(p) => (false, p)
    case Clo(params, body, env) => (false, "<lambda>")
    case Evalfun(i) => (false, "<evalfun"+i+">")
    case Cont(i) => (false, "<cont"+i+">")
    case Cell(key) => pp(cells(key))
    case Code(c) => (false, "{"+c+"}")
  }
  def display(v: Value) = println(addParen(pp(v)))

  def top_eval[R[_]:Ops](exp: Value): R[Value] = {
    try {
      base_eval[R](exp, init_env, mkCont[R](x => x))
    } finally {
      reset()
    }
  }
}

import eval._
import scala.lms.common._

trait EvalDsl extends Functions with TupleOps with IfThenElse with Equal with UncheckedOps {
  def base_apply_rep(f: Rep[Value], args: Rep[Value], env: Value, cont: Value): Rep[Value]
  def make_fun_rep(f: Fun[Rep]): Rep[Value]
  implicit object OpsRep extends scala.Serializable with Ops[Rep] {
    def lift(v: Value) = unit(v)
    def app(f: Rep[Value], args: Rep[Value], env: Value, cont: Value) =
      base_apply_rep(f, args, env, cont)
    def isTrue(v: Rep[Value]): Rep[Boolean] = unchecked("isTrue(", v, ")")//unit(B(false))!=v
    def ifThenElse[A:Manifest](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A] = if (cond) thenp else elsep
    def makeFun(f: Fun[Rep]) = make_fun_rep(f)
    def makePair(car: Rep[Value], cdr: Rep[Value]) =
      unchecked("makePair(", car, ", ", cdr, ")")
    def getCar(p: Rep[Value]) = unchecked("getCar(", p, ")")
    def getCdr(p: Rep[Value]) = unchecked("getCdr(", p, ")")
    def cellNew(v: Rep[Value]) = unchecked("Code(cellNew(", v, "))")
    def cellRead(c: Rep[Value]) = unchecked("cellRead(", c, ")")
    def cellSet(c: Rep[Value], v: Rep[Value]) = unchecked("cellSet(", c, ", ", v, ")")
    def inRep = true
  }

  def snippet(v: Rep[Value]): Rep[Value]
}

trait EvalDslExp extends EvalDsl with EffectExp with FunctionsExp with TupleOpsExp with IfThenElseExp with EqualExp with UncheckedOpsExp {
  case class BaseApplyRep(f: Rep[Value], args: Rep[Value], env: Value, cont: Rep[Value => Value]) extends Def[Value]
  def base_apply_rep(f: Rep[Value], args: Rep[Value], env: Value, cont: Value): Rep[Value] = cont match {
    case Cont(key) => reflectEffect(BaseApplyRep(f, args, env, fun(conts(key).fun[Rep])))
  }

  case class EvalfunRep(x: Sym[Value], y: Block[Value]) extends Def[Value]
  def make_fun_rep(f: Fun[Rep]) = {
    val x = fresh[Value]
    val y = reifyEffects{
      val fn = f.fun[Rep]
      fn(x)
    }
    reflectEffect(EvalfunRep(x, y))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case EvalfunRep(x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }
}

trait EvalDslGen extends ScalaGenFunctions with ScalaGenTupleOps with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenUncheckedOps {
  val IR: EvalDslExp
  import IR._

  def hasCode(v: Value): Boolean = v match {
    case Code(c) => true
    case P(a, b) => hasCode(a) || hasCode(b)
    case Clo(a, b, c) => hasCode(a) || hasCode(b) || hasCode(c)
    case _ => false
  }
  override def quote(x: Exp[Any]) : String = x match {
    case Const(P(a, b)) => "P("+quote(Const(a))+", "+quote(Const(b))+")"
    case Const(Code(c)) => quote(c.asInstanceOf[Rep[Any]])
    case Const(Clo(param, body, env)) =>  "Clo("+quote(Const(param))+", "+quote(Const(body))+", "+quote(Const(env))+")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BaseApplyRep(f, args, env, cont) =>
      emitValDef(sym, "base_apply[R]("+quote(f)+", "+quote(args)+", "+quote(Const(env))+", mkCont[R]("+quote(cont)+"))")
    case EvalfunRep(x, y) =>
      stream.println("val f_"+quote(sym)+" = {(" + quote(x) + ": Value) => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": R[Value]")
      stream.println("}")
      emitValDef(sym, "evalfun (new Fun[NoRep] { def fun[R[_]:Ops] = f_"+quote(sym)+".asInstanceOf[Value => R[Value]] })")
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = ifThenElse((" + quote(c) + "), {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("}, {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("})")
    case _ => super.emitNode(sym, rhs)
  }
}

trait EvalDslImpl extends EvalDslExp { q =>
  val codegen = new EvalDslGen {
    val IR: q.type = q

    override def remap[A](m: Manifest[A]): String = {
      val s = m.toString
      if (s=="scala.lms.black.eval$Value") "R[Value]"
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

        stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends Fun[NoRep] with (Value => Value) {")

        stream.println("def apply(v: Value): Value = v")
        stream.println("def fun[R[_]:Ops] = { v => fun[R](v)  }")
        stream.println("def fun[R[_]:Ops]("+args.map(a => quote(a) + ":" + "Value"/*remap(a.tp)*/).mkString(", ")+"): "+sA+" = {")
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
  dumpGeneratedCode = true
  lazy val f = compile(snippet).asInstanceOf[Fun[NoRep]]
  def precompile: Unit = f
  def precompileSilently: Unit = utils.devnull(f)
  def eval[R[_]:Ops](v: Value): R[Value] = {
    val fn = f.fun[R]
    fn(v)
  }
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
  }
}
