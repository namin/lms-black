package scala.lms.black

import eval._

object printer {
  type PrintRep[A] = String
  var id_count = 0
  var pending: List[String] = Nil
  implicit object OpsPrinter extends Ops[PrintRep] {
    def freshSuffix: String = {
      id_count += 1
      s"_$id_count"
    }
    def _lift(v: Value) = v match {
      case Cell(key) => s"<cell $key>"
      case Code(c: String) => c
      case _ => "`"+show(v)
    }
    def _liftb(b: Boolean) = b.toString
    def _unlift(v: String) = Code[PrintRep](v)
    def _app(fun: String, args: String, cont: Value) =
      s"_app($fun, $args, ${_lift(cont)})"
    def _true(v: String) = s"_true($v)"
    def _if(cond: String, thenp: => String, elsep: => String) =
      s"_if($cond,\n$thenp,\n$elsep)"
    def _fun(f: Fun[PrintRep]) = {
      val fn = f.fun[PrintRep]
      val id = freshSuffix
      val kxs = "kxs"+id
      s"_fun{$kxs =>\n${fn(kxs)}}"
    }
    def _cont(f: FunC[PrintRep]) = {
      val fn = f.fun[PrintRep]
      val id = freshSuffix
      val v = "v"+id
      val old_pending = pending
      pending = Nil
      val r = fn(v)
      val s = pending.reverse.mkString("\n")
      val x = if (s.isEmpty) "" else "\n"
      pending = old_pending
      Code[PrintRep](s"_cont{$v =>\n$s$x$r}")
    }
    def _cons(car: String, cdr: String) = s"_cons($car, $cdr)"
    def _car(p: String) = s"_car($p)"
    def _cdr(p: String) = s"_cdr($p)"
    def _cell_new(v: String, memo: String) = s"_cell_new($v, $memo)"
    def _cell_read(c: String) = s"_cell_read($c)"
    def _cell_set(c: String, v: String) = {
      pending = s"_cell_set($c, $v)"::pending
      ""
    }
    def inRep = false
  }

  def summarize(f: Evalfun) = {
    id_count = 0
    val fn = funs(f.key).fun[PrintRep]
    val body =
      fn(P(Code[PrintRep]("k"), Code[PrintRep]("xs"))).
        replace("_cdr(`({k} . {xs}))", "xs").
        replace("_car(`({k} . {xs}))", "k")
    val r = s"{(k, xs) =>\n$body}"
    assert(pending == Nil)
    r
  }
}
