package scala.lms.black

import eval._
import scala.lms.common._

trait EvalDsl extends IfThenElse with LiftBoolean {
  implicit def valTyp: Typ[Value]
  implicit def boolTyp: Typ[Boolean]
  def base_apply_rep(m: MEnv, f: Rep[Value], args: Rep[Value], env: Value, cont: Value): Rep[Value]
  def make_fun_rep(m: MEnv, f: Fun[Rep]): Rep[Value]
  def if_then_else_rep[A:Typ](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A]
  def get_car_rep(p: Rep[Value]): Rep[Value]
  def get_cdr_rep(p: Rep[Value]): Rep[Value]
  def make_pair_rep(car: Rep[Value], cdr: Rep[Value]): Rep[Value]
  def is_true_rep(cond: Rep[Value]): Rep[Boolean]
  def cell_new_rep(v: Rep[Value]): Rep[Value]
  def cell_read_rep(c: Rep[Value]): Rep[Value]
  def cell_set_rep(c: Rep[Value], v: Rep[Value]): Rep[Value]
  implicit object OpsRep extends scala.Serializable with Ops[Rep] {
    type Tag[A] = Typ[A]
    def valueTag = typ[Value]
    def lift(v: Value) = unit(v)
    def app(m: MEnv, f: Rep[Value], args: Rep[Value], env: Value, cont: Value) =
      base_apply_rep(m, f, args, env, cont)
    def isTrue(v: Rep[Value]): Rep[Boolean] = is_true_rep(v)
    def ifThenElse[A:Tag](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A] = if_then_else_rep(cond, thenp, elsep)
    def makeFun(m: MEnv, f: Fun[Rep]) = make_fun_rep(m, f)
    def makePair(car: Rep[Value], cdr: Rep[Value]) = make_pair_rep(car, cdr)
    def getCar(p: Rep[Value]) = get_car_rep(p)
    def getCdr(p: Rep[Value]) = get_cdr_rep(p)
    def cellNew(v: Rep[Value]) = cell_new_rep(v)
    def cellRead(c: Rep[Value]) = cell_read_rep(c)
    def cellSet(c: Rep[Value], v: Rep[Value]) = cell_set_rep(c, v)
    def inRep = true
  }

  def snippet(v: Rep[Value]): Rep[Value]
}

trait EvalDslExp extends EvalDsl with EffectExp with IfThenElseExp {
  implicit def valTyp: Typ[Value] = manifestTyp
  implicit def boolTyp: Typ[Boolean] = manifestTyp

  case class CarRep(p: Rep[Value]) extends Def[Value]
  case class CdrRep(p: Rep[Value]) extends Def[Value]
  case class MakePairRep(car: Rep[Value], cdr: Rep[Value]) extends Def[Value]
  case class IsTrueRep(cond: Rep[Value]) extends Def[Boolean]
  case class CellReadRep(c: Rep[Value]) extends Def[Value]
  case class CellSetRep(c: Rep[Value], v: Rep[Value]) extends Def[Value]
  case class CellNewRep(v: Rep[Value]) extends Def[Value]

  def make_pair_rep(car: Rep[Value], cdr: Rep[Value]) = (car, cdr) match {
    case (Const(a), Const(b)) => Const[Value](P(a, b))
    case _ => MakePairRep(car, cdr)
  }

  def is_true_rep(cond: Rep[Value]) = cond match {
    case Const(B(b)) => Const(b)
    case _ => IsTrueRep(cond)
  }
  var cell_es = Map[Rep[Value], Rep[Value]]()
  def cell_new_rep(v: Rep[Value]) = {
    val c = reflectEffect(CellNewRep(v))
    cell_es += (c -> v)
    c
  }
  def cell_read_rep(c: Rep[Value]) = reflectEffect(CellReadRep(c))
  def cell_set_rep(c: Rep[Value], v: Rep[Value]) = {
    val uc = c match {
      case Const(Code(uc: Rep[Value])) => uc
      case _ => c
    }
    cell_es -= uc
    reflectEffect(CellSetRep(c, v))
  }

  def get_car_rep(p: Rep[Value]) = p match {
    case Const(P(a, b)) => Const(a)
    case _ => CarRep(p)
  }
  def get_cdr_rep(p: Rep[Value]) = p match {
    case Const(P(a, b)) => Const(b)
    case _ => CdrRep(p)
  }

  def if_then_else_rep[A:Typ](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case _ => if (cond) thenp else elsep
  }
  case class BaseApplyRep(m: MEnv, f: Rep[Value], args: Rep[Value], env: Value, cont_x: Sym[Value], cont_y: Block[Value]) extends Def[Value]

  def hasCode(v: Value): Boolean = v match {
    case Code(c) => true
    case P(a, b) => hasCode(a) || hasCode(b)
    case Clo(a, b, c) => hasCode(a) || hasCode(b) || hasCode(c)
    case _ => false
  }
  var omit_reads = Set[Rep[Value]]()
  def base_apply_rep(m: MEnv, f: Rep[Value], args: Rep[Value], env: Value, cont: Value): Rep[Value] = (f, args, cont) match {
    case (Const(fprim@Prim(p)), Const(vs@P(_, _)), Cont(key)) if !primitive_with_side_effect.contains(fprim) && !hasCode(vs) =>
      val r = apply_primitive(p, vs)
      val fn = conts(key).fun[Rep]
      fn(Const(r))
    case (Def(Reflect(CellReadRep(Const(Cell(cid))), _, _)), Const(vs@P(a, P(_, P(_, N)))), Cont(key)) if !hasCode(a) =>
      omit_reads += f
      val Evalfun(ekey) = cells(cid)
      val efn = funs(ekey).fun[Rep]
      val r = efn(MEnv(env, m))(vs)
      val fn = conts(key).fun[Rep]
      fn(r)
    case (Const(fcont@Cont(_)), Def(MakePairRep(a, Const(N))), _) =>
      apply_cont[Rep](m, env, fcont, a)
    case (_, _, Cont(key)) => 
      val x = fresh[Value]
      val y = reifyEffects{
        val fn = conts(key).fun[Rep]
        fn(x)
      }
      reflectEffect(BaseApplyRep(m, f, args, env, x, y))
  }

  case class EvalfunRep(x: Sym[Value], y: Block[Value]) extends Def[Value]
  def make_fun_rep(m: MEnv, f: Fun[Rep]) = {
    val x = fresh[Value]
    val y = reifyEffects{
      val fn = f.fun[Rep]
      fn(m)(x)
    }
    reflectEffect(EvalfunRep(x, y))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case BaseApplyRep(m, f, a, env, x, y) => syms(x) ::: effectSyms(y)
    case EvalfunRep(x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }
}

trait EvalDslGen extends ScalaGenIfThenElse {
  val IR: EvalDslExp
  import IR._

  var rs = List[String]("")
  def quote_R(a: String): String = "R"+a
  def quote_R(rs: List[String]): String = quote_R(rs.head)
  def quoteR = quote_R(rs)
  def quoteO = "o"+(rs.head.toString)
  def quote_Ev(a: String, b: String): String = "ev"+b+"_"+a
  def quote_Ev(rs: List[String]): String = if (rs.tail.isEmpty) "ev"+(rs.head.toString) else quote_Ev(rs.head, rs.tail.head)
  def quoteEv = quote_Ev(rs)

  def quoteL(x: Exp[Any]) : String = x match {
    case Const(Code(e: Exp[Any])) => quote(e)
    case Const(v: Value) => quoteO+".lift("+quote(x)+")"
    case _ => quote(x)
  }
  def quoteInP(x: Value) : String = x match {
    case Code(_) => "Code("+quote(Const(x))+")"
    case _ => quote(Const(x))
  }
  override def quote(x: Exp[Any]) : String = x match {
    case Const(P(a, b)) => "P("+quoteInP(a)+", "+quoteInP(b)+")"
    case Const(Code(c)) => if (c.isInstanceOf[Rep[Any]]) quote(c.asInstanceOf[Rep[Any]]) else quote(Const(c.asInstanceOf[Value]))
    case Const(Clo(param, body, env)) =>  "Clo("+quote(Const(param))+", "+quote(Const(body))+", "+quote(Const(env))+")"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CarRep(p) => emitValDef(sym, quoteO+".getCar("+quoteL(p)+")")
    case CdrRep(p) => emitValDef(sym, quoteO+".getCdr("+quoteL(p)+")")
    case IsTrueRep(cond) => emitValDef(sym, quoteO+".isTrue("+quoteL(cond)+")")
    case CellSetRep(c, v) => emitValDef(sym, quoteO+".cellSet("+quoteL(c)+", "+quoteL(v)+")")
    case MakePairRep(a, b) => emitValDef(sym, quoteO+".makePair("+quoteL(a)+", "+quoteL(b)+")")
    case CellReadRep(c) => if (!omit_reads.contains(sym.asInstanceOf[Exp[Value]]))
      emitValDef(sym, cell_es.get(c) match {
      case Some(v) => quoteL(v)
      case None => quoteO+".cellRead("+quoteL(c)+")"
    })
    case CellNewRep(v) => emitValDef(sym, cell_es.get(sym.asInstanceOf[Exp[Value]]) match {
      case Some(_) => quoteL(v)
      case None => quoteO+".cellNew("+quoteL(v)+")"
    })
    case BaseApplyRep(m, f, args, env, cont_x, cont_y) =>
      emitValDef(sym, quoteO+".app("+m+", "+quoteL(f)+", "+quoteL(args)+", "+quote(Const(env))+", mkCont["+quoteR+"]{("+quote(cont_x)+": "+quoteR+"[Value]) =>")
      emitBlock(cont_y)
      stream.println(quoteL(getBlockResult(cont_y)) + ": "+quoteR+"[Value]")
      stream.println("})")
    case EvalfunRep(x, y) =>
      val r1 = quoteR
      val oldO = quoteO
      val a = rs.size.toString
      rs = a::rs
      val r2 = quoteR
      emitValDef(sym, oldO+".makeFun(m, new Fun["+r1+"] { def fun["+r2+"[_]:Ops](implicit "+quoteEv+": Convert["+r1+","+r2+"]) = { (m: MEnv) => {(" + quote(x) + ": "+r1+"[Value]) => ")
      stream.println("val "+quoteO+" = implicitly[Ops["+r2+"]]")
      stream.println("implicit def convert_"+quoteEv+"(x: "+r1+"[Value]): "+r2+"[Value] = "+quoteEv+".convert(x)")
      if (!rs.tail.isEmpty) {
        for ((b, c) <- rs.tail.zip(rs.tail.tail)) {
          stream.println("val "+quote_Ev(a, c)+": Convert["+quote_R(c)+","+quote_R(a)+"] = convertTrans["+quote_R(c)+","+quote_R(b)+","+quote_R(a)+"]("+quote_Ev(b, c)+", "+quote_Ev(a, b)+")")
          stream.println("implicit def convert_"+quote_Ev(a, c)+"(x: "+quote_R(c)+"[Value]): "+quote_R(a)+"[Value] = "+quote_Ev(a, c)+".convert(x)")
        }
      }
      emitBlock(y)
      stream.println(quoteL(getBlockResult(y)) + ": "+r2+"[Value]")
      rs = rs.tail
      stream.println("}}})")
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = "+quoteO+".ifThenElse((" + quote(c) + "), {")
      emitBlock(a)
      stream.println(quoteL(getBlockResult(a)))
      stream.println("}, {")
      emitBlock(b)
      stream.println(quoteL(getBlockResult(b)))
      stream.println("})("+quoteO+".valueTag)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait EvalDslImpl extends EvalDslExp { q =>
  val codegen = new EvalDslGen {
    val IR: q.type = q

    override def remap[A](m: Typ[A]): String = {
      val s = m.toString
      if (s=="scala.lms.black.eval$Value") "R[Value]"
      else super.remap(m)
    }

    override def emitFileHeader() {
      stream.println("import language.higherKinds")
      stream.println("import scala.lms.black.eval._")
    }

    override def emitSource[A:Typ](args: List[Sym[_]], body: Block[A], className: String, out: java.io.PrintWriter) = {
      val sA = remap(typ[A])

      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println("/*****************************************\n"+
                       "  Emitting Generated Code                  \n"+
                       "*******************************************/")
        emitFileHeader()

        stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends Fun[NoRep] with (Value => Value) {")

        stream.println("def apply(v: Value): Value = v")
        stream.println("def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { (m: MEnv) => { v => fun[R](m, v)(implicitly[Ops[R]], ev)  } }")
        stream.println("def fun[R[_]:Ops](m: MEnv, "+args.map(a => quote(a) + ":" + "Value"/*remap(a.tp)*/).mkString(", ")+")(implicit ev: Convert[NoRep,R]): "+sA+" = {")
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
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
    source.toString
  }
}
