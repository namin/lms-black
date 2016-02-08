package scala.lms.black

import eval._
import scala.lms.common._

trait EvalDsl extends IfThenElse with LiftBoolean {
  implicit def valTyp: Typ[Value]
  implicit def boolTyp: Typ[Boolean]
  def app_rep(f: Rep[Value], args: Rep[Value], cont: Value): Rep[Value]
  def fun_rep(f: Fun[Rep]): Rep[Value]
  def cont_rep(c: CodeCont[Rep], k: FunC[Rep]): Rep[Value]
  def if_rep[A:Typ](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A]
  def car_rep(p: Rep[Value]): Rep[Value]
  def cdr_rep(p: Rep[Value]): Rep[Value]
  def cons_rep(car: Rep[Value], cdr: Rep[Value]): Rep[Value]
  def true_rep(cond: Rep[Value]): Rep[Boolean]
  def cell_new_rep(v: Rep[Value], s: String): Rep[Value]
  def cell_read_rep(c: Rep[Value]): Rep[Value]
  def cell_set_rep(c: Rep[Value], v: Rep[Value]): Rep[Value]
  implicit object OpsRep extends scala.Serializable with Ops[Rep] {
    type Tag[A] = Typ[A]
    def valueTag = typ[Value]
    def _lift(v: Value) = v match {
      case k@CodeCont(_, _) => k.asInstanceOf[CodeCont[Rep]].force
      case _ => unit(v)
    }
    def _unlift(v: Rep[Value]) = Code(v)
    def _app(f: Rep[Value], args: Rep[Value], cont: Value) = app_rep(f, args, cont)
    def _true(v: Rep[Value]): Rep[Boolean] = true_rep(v)
    def _if[A:Tag](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]): Rep[A] = if_rep(cond, thenp, elsep)
    def _fun(f: Fun[Rep]) = fun_rep(f)
    def _cont(f: FunC[Rep]) = {
      lazy val k: CodeCont[Rep] = CodeCont[Rep](f, () => cont_rep(k, f))
      k
    }
    def _cons(car: Rep[Value], cdr: Rep[Value]) = cons_rep(car, cdr)
    def _car(p: Rep[Value]) = car_rep(p)
    def _cdr(p: Rep[Value]) = cdr_rep(p)
    def _cell_new(v: Rep[Value], s: String) = cell_new_rep(v, s)
    def _cell_read(c: Rep[Value]) = cell_read_rep(c)
    def _cell_set(c: Rep[Value], v: Rep[Value]) = cell_set_rep(c, v)
    def inRep = true
  }

  def snippet(v: Rep[Value]): Rep[Value]
}

trait EvalDslExp extends EvalDsl with EffectExp with IfThenElseExp {
  implicit def valTyp: Typ[Value] = manifestTyp
  implicit def boolTyp: Typ[Boolean] = manifestTyp
  implicit def stringTyp: Typ[String] = manifestTyp

  case class CarRep(p: Rep[Value]) extends Def[Value]
  case class CdrRep(p: Rep[Value]) extends Def[Value]
  case class ConsRep(car: Rep[Value], cdr: Rep[Value]) extends Def[Value]
  case class TrueRep(cond: Rep[Value]) extends Def[Boolean]
  case class CellReadRep(c: Rep[Value]) extends Def[Value]
  case class CellSetRep(c: Rep[Value], v: Rep[Value]) extends Def[Value]
  case class CellNewRep(v: Rep[Value], s: String) extends Def[Value]

  def cons_rep(car: Rep[Value], cdr: Rep[Value]) = (car, cdr) match {
    case (Const(a), Const(b)) => Const[Value](P(a, b))
    case (Def(ContRep(c, _, _, _)), Const(N)) => Const[Value](P(c, N))
    case _ => ConsRep(car, cdr)
  }

  def true_rep(cond: Rep[Value]) = cond match {
    case Const(B(b)) => Const(b)
    case _ => TrueRep(cond)
  }

  var cell_es = Map[Rep[Value], Rep[Value]]()
  def cell_new_rep(v: Rep[Value], s: String) = {
    val c = reflectEffect(CellNewRep(v, s))
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

  def car_rep(p: Rep[Value]) = p match {
    case Const(P(a, b)) => OpsRep._lift(a)
    case _ => CarRep(p)
  }
  def cdr_rep(p: Rep[Value]) = p match {
    case Const(P(a, b)) => OpsRep._lift(b)
    case _ => CdrRep(p)
  }

  def if_rep[A:Typ](cond: Rep[Boolean], thenp: => Rep[A], elsep: => Rep[A]) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case _ => if (cond) thenp else elsep
  }

  def hasCode(v: Value): Boolean = v match {
    case Code(c) => true
    case P(a, b) => hasCode(a) || hasCode(b)
    case Clo(a, b, c, _) => hasCode(a) || hasCode(b) || hasCode(c)
    case _ => false
  }

  case class ContRep(c: CodeCont[Rep], k: FunC[Rep], x: Sym[Value], y: Block[Value]) extends Def[Value]
  def cont_rep(c: CodeCont[Rep], k: FunC[Rep]): Rep[Value] = {
    val x = fresh[Value]
    val y = reifyEffects{
      val fn = k.fun[Rep]
      fn(x)
    }
    ContRep(c, k, x, y)
  }

  var omit_reads = Set[Rep[Value]]()
  case class AppRep(f: Rep[Value], args: Rep[Value], cont_x: Sym[Value], cont_y: Block[Value]) extends Def[Value]

  def app_rep(f: Rep[Value], args: Rep[Value], cont: Value): Rep[Value] = (f, args) match {
    case (Const(fprim@Prim(p)), Const(vs@P(_, _))) if !effectful_primitives.contains(fprim) && !hasCode(vs) =>
      val r = apply_primitive(p, vs)
      apply_cont[Rep](cont, OpsRep._lift(r))
    case (Def(Reflect(CellReadRep(Const(Cell(cid))), _, _)), Const(vs@P(a, P(_, P(_, N))))) if !hasCode(a) =>
      omit_reads += f
      val Evalfun(ekey) = cells(cid)
      val efn = funs(ekey).fun[Rep]
      val r = efn(vs)
      apply_cont[Rep](cont, r)
    case (Const(Evalfun(ekey)), Const(vs@P(a, P(_, P(_, N))))) if !hasCode(a) =>
      val efn = funs(ekey).fun[Rep]
      val r = efn(vs)
      apply_cont[Rep](cont, r)
    case (Def(ContRep(_, k, _, _)), Def(ConsRep(a, Const(N)))) =>
      val fn = k.fun[Rep]
      val r = fn(a)
      apply_cont[Rep](cont, r)
    case (Const(fcont), Def(ConsRep(a, Const(N)))) if isCont(fcont) =>
      apply_cont[Rep](cont, apply_cont[Rep](fcont, a))
    case (_, _) =>
      //println("//DEBUG "+f+" applied to "+args)
      val x = fresh[Value]
      val y = reifyEffects{
        val Some(fn) = cont2fun[Rep](cont)
        fn(x)
      }
      reflectEffect(AppRep(f, args, x, y))
  }

  case class EvalfunRep(x: Sym[Value], y: Block[Value]) extends Def[Value]
  def fun_rep(f: Fun[Rep]) = {
    val x = fresh[Value]
    val y = reifyEffects{
      val fn = f.fun[Rep]
      fn(x)
    }
    reflectEffect(EvalfunRep(x, y))
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case AppRep(f, a, x, y) => syms(x) ::: effectSyms(y)
    case EvalfunRep(x, y) => syms(x) ::: effectSyms(y)
    case ContRep(_, _, x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Code(v) => syms(v)
    case c@CodeCont(_, _) => syms(c.force)
    case AppRep(f, a, x, y) => syms(f) ::: syms(a) ::: syms(y)
    case EvalfunRep(x, y) => syms(y)
    case ContRep(_, _, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Code(v) => symsFreq(v)
    case c@CodeCont(_, _) => symsFreq(c.force)
    case AppRep(f, a, x, y) => freqNormal(f) ::: freqNormal(a) ::: freqHot(y)
    case EvalfunRep(x, y) => freqHot(y)
    case ContRep(_, _, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
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
    case Const(v: Value) => quoteO+"._lift("+quote(x)+")"
    case _ => quote(x)
  }
  def quoteInP(x: Value) : String = x match {
    case Code(_) => quoteO+"._unlift("+quote(Const(x))+")"
    case c@CodeCont(_, _) =>  quoteO+"._unlift("+quote(c.asInstanceOf[CodeCont[Rep]].force)+")"
    case _ => quote(Const(x))
  }
  override def quote(x: Exp[Any]) : String = x match {
    case Const(P(a, b)) => "P("+quoteInP(a)+", "+quoteInP(b)+")"
    case Const(Code(c)) => if (c.isInstanceOf[Rep[Any]]) quote(c.asInstanceOf[Rep[Any]]) else quote(Const(c.asInstanceOf[Value]))
    case Const(Clo(param, body, env, m)) =>  "Clo("+quote(Const(param))+", "+quote(Const(body))+", "+quote(Const(env))+", "+m.toString+")"
    case _ => super.quote(x)
  }
  def print_cont(x: Sym[Value], y: Block[Value]) = {
    val r1 = quoteR
    val oldO = quoteO
    val a = rs.size.toString
    rs = a::rs
    val r2 = quoteR
    stream.println(oldO+"._cont(new FunC["+r1+"] { def fun["+r2+"[_]:Ops](implicit "+quoteEv+": Convert["+r1+","+r2+"]) = {(" + quote(x) + ": "+r2+"[Value]) => ")
    if (!rs.tail.isEmpty && getBlockResult(y)!=x) {
      stream.print("val "+quoteO+" = implicitly[Ops["+r2+"]]; ")
      stream.print("implicit def convert_"+quoteEv+"(x: "+r1+"[Value]): "+r2+"[Value] = "+quoteEv+".convert(x); ")
      for ((b, c) <- rs.tail.zip(rs.tail.tail)) {
        stream.print("val "+quote_Ev(a, c)+": Convert["+quote_R(c)+","+quote_R(a)+"] = convertTrans["+quote_R(c)+","+quote_R(b)+","+quote_R(a)+"]("+quote_Ev(b, c)+", "+quote_Ev(a, b)+"); ")
        stream.print("implicit def convert_"+quote_Ev(a, c)+"(x: "+quote_R(c)+"[Value]): "+quote_R(a)+"[Value] = "+quote_Ev(a, c)+".convert(x); ")
      }
      stream.println("")
    }
    emitBlock(y)
    stream.println(quoteL(getBlockResult(y)) + ": "+r2+"[Value]")
    rs = rs.tail
    stream.println("}})")
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CarRep(p) => emitValDef(sym, quoteO+"._car("+quoteL(p)+")")
    case CdrRep(p) => emitValDef(sym, quoteO+"._cdr("+quoteL(p)+")")
    case TrueRep(cond) => emitValDef(sym, quoteO+"._true("+quoteL(cond)+")")
    case CellSetRep(c, v) => emitValDef(sym, quoteO+"._cell_set("+quoteL(c)+", "+quoteL(v)+")")
    case ConsRep(a, b) => emitValDef(sym, quoteO+"._cons("+quoteL(a)+", "+quoteL(b)+")")
    case CellReadRep(c) => if (!omit_reads.contains(sym.asInstanceOf[Exp[Value]]))
      emitValDef(sym, cell_es.get(c) match {
      case Some(v) => quoteL(v)
      case None => quoteO+"._cell_read("+quoteL(c)+")"
    })
    case CellNewRep(v, s) => emitValDef(sym, cell_es.get(sym.asInstanceOf[Exp[Value]]) match {
      case Some(_) => quoteL(v)
      case None => quoteO+"._cell_new("+quoteL(v)+", "+quote(Const(s))+")"
    })
    case AppRep(f, args, x, y) =>
      emitValDef(sym, quoteO+"._app("+quoteL(f)+", "+quoteL(args)+", ")
      print_cont(x, y)
      stream.println(")")
    case EvalfunRep(x, y) =>
      val r1 = quoteR
      val oldO = quoteO
      val a = rs.size.toString
      rs = a::rs
      val r2 = quoteR
      emitValDef(sym, oldO+"._fun(new Fun["+r1+"] { def fun["+r2+"[_]:Ops](implicit "+quoteEv+": Convert["+r1+","+r2+"]) = {(" + quote(x) + ": "+r1+"[Value]) => ")
      stream.println("val "+quoteO+" = implicitly[Ops["+r2+"]]")
      stream.println("implicit def convert_"+quoteEv+"(x: "+r1+"[Value]): "+r2+"[Value] = "+quoteEv+".convert(x)")
      if (!rs.tail.isEmpty) {
        for ((b, c) <- rs.tail.zip(rs.tail.tail)) {
          stream.print("val "+quote_Ev(a, c)+": Convert["+quote_R(c)+","+quote_R(a)+"] = convertTrans["+quote_R(c)+","+quote_R(b)+","+quote_R(a)+"]("+quote_Ev(b, c)+", "+quote_Ev(a, b)+"); ")
          stream.print("implicit def convert_"+quote_Ev(a, c)+"(x: "+quote_R(c)+"[Value]): "+quote_R(a)+"[Value] = "+quote_Ev(a, c)+".convert(x); ")
        }
        stream.println("")
      }
      emitBlock(y)
      stream.println(quoteL(getBlockResult(y)) + ": "+r2+"[Value]")
      rs = rs.tail
      stream.println("}})")
    case ContRep(_, _, x, y) =>
      emitValDef(sym, quoteO+"._lift(")
      print_cont(x, y)
      stream.println(")")
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = "+quoteO+"._if((" + quoteL(c) + "), {")
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
      stream.println("import language.implicitConversions")
      stream.println("import scala.lms.black.eval._")
    }

    // this is cargo culted from the `ScalaCodegen`
    override def emitSource[A:Typ](args: List[Sym[_]], body: Block[A], className: String, out: java.io.PrintWriter) = {
      val sA = remap(typ[A])

      val staticData = getFreeDataBlock(body)

      withStream(out) {
        stream.println("/*****************************************\n"+
                       "  Emitting Generated Code                  \n"+
                       "*******************************************/")
        emitFileHeader()

        stream.println("class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tp).mkString(",")+")")+" extends Fun[NoRep] with (Value => Value) {")

        // dummy function for CompileScala below to be happy casting
        stream.println("def apply(v: Value): Value = v")

        stream.println("def fun[R[_]:Ops](implicit ev: Convert[NoRep,R]) = { v => fun[R](v)(implicitly[Ops[R]], ev)  }")

        stream.println("def fun[R[_]:Ops]("+args.map(a => quote(a) + ":" + "Value"/*remap(a.tp)*/).mkString(", ")+")(implicit ev: Convert[NoRep,R]): "+sA+" = {")
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
