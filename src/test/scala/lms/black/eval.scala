package scala.lms.black

import eval._

class TestEvaluator extends TestSuite {
  val under = "eval_"

  def list2pair(xs: List[Value]): Value = xs match {
    case Nil => N
    case (x::xs) => P(x, list2pair(xs))
  }
  def If(cond: Value, thenp: Value, elsep: Value) =
    P(S("if"), P(cond, P(thenp, P(elsep, N))))
  def A(f: Value, args: List[Value]) =
    P(f, list2pair(args))
  def V(sym: String) = S(sym)
  def L(compile: Boolean, param: String, body: Value) =
    P(S((if (compile) "c" else "")+"lambda"), P(P(S(param), N), P(body, N)))

  def ex_id(c: Boolean) = A(L(c, "x", V("x")), List(I(1)))
  test ("id evaluated") {
    assertResult(I(1)){top_eval[NoRep](ex_id(false))}
  }

  test ("id compiled") {
    //checkOut("id", "scala",
      assertResult(I(1)){top_eval[NoRep](ex_id(true))}
    //)
  }

  def Y(c: Boolean) = L(c, "fun", A(L(c, "F", A(V("F"), List(V("F")))), List(L(c, "F", A(V("fun"), List(L(c, "x", A(A(V("F"), List(V("F"))), List(V("x"))))))))))
  def fib(c: Boolean) = L(c, "fib", L(c, "n", If(A(Prim("<"), List(V("n"), I(2))), V("n"), A(Prim("+"), List(A(V("fib"), List(A(Prim("-"), List(V("n"), I(1))))), A(V("fib"), List(A(Prim("-"), List(V("n"), I(2))))))))))
  def sumf(c: Boolean) = L(c, "f", L(c, "sumf", L(c, "n", If(A(Prim("<"), List(V("n"), I(0))), I(0), A(Prim("+"), List(A(V("f"), List(V("n"))), A(V("sumf"), List(A(Prim("-"), List(V("n"), I(1)))))))))))

  test ("fib 7 evaluated") {
    assertResult(I(13)){
      top_eval[NoRep](A(A(Y(false), List(fib(false))), List(I(7))))}
  }

  test ("fib 7 compiled fib") {
    //checkOut("yfibc", "scala",
      assertResult(I(13)){
        top_eval[NoRep](A(A(Y(false), List(fib(true))), List(I(7))))}
    //)
  }

  test ("fib 7 compiled all") {
    //checkOut("ycfibc", "scala",
      assertResult(I(13)){
        top_eval[NoRep](A(A(Y(true), List(fib(true))), List(I(7))))}
    //)
  }

  test ("sum of fibs evaluated") {
    assertResult(I(33)){
      top_eval[NoRep](A(A(Y(false), List(A(sumf(false), List(A(Y(false), List(fib(false))))))), List(I(7))))}
  }

  test ("sum of fibs compiled") {
    assertResult(I(33)){
      top_eval[NoRep](A(A(Y(true), List(A(sumf(true), List(A(Y(true), List(fib(true))))))), List(I(7))))}
  }
}
