package scala.lms.black

import eval._
import scala.util.parsing.combinator._

object parser extends JavaTokenParsers with PackratParsers {
    def exp: Parser[Value] =
      "#f" ^^ { case _ => B(false) } |
      "#t" ^^ { case _ => B(true) } |
      "[0-9]+".r ^^ { case s => I(s.toInt) } |
      """[^\s\(\)]+""".r ^^ { case s => S(s) } |
      "()" ^^ { case _ => N } |
      "(" ~> exps <~ ")" ^^ { case vs => vs }

  def exps: Parser[Value] =
      exp ~ exps ^^ { case v~vs => P(v, vs) } |
      exp ^^ { case v => P(v, N) }
}
