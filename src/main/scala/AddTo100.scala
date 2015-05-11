import scala.collection.immutable.IndexedSeq
import scala.util.parsing.combinator.RegexParsers

/**
 * A parser that handles expressions like "1.2+3" and interprets them like "12+3". It introduces
 * an operator in addition to "+" and "-", the combiner operator ".". The combiner operator has
 * higher precedence than addition and subtraction, and concatenates its arguments together
 * as strings; e.g. 1 . 2 == 12
 */
trait Parser extends RegexParsers {
  def number: Parser[Int] = "[\\d]+".r ^^ { case n => n.toInt }

  def combiner: Parser[Int => Int] = "." ~> number ^^ { case num => (i: Int) => s"$i$num".toInt }

  def combined: Parser[Int] = number ~ combiner ^^ { case num ~ comb => comb(num) }

  def subexpr: Parser[Int] = (combined | number) ~ rep(combiner) ^^ { case x ~ y => (x /: y)((acc, f) => f(acc)) }

  def adder: Parser[Int => Int] = "+" ~> subexpr ^^ { case sub => (i: Int) => i + sub }

  def subtractor: Parser[Int => Int] = "-" ~> subexpr ^^ { case sub => (i: Int) => i - sub }

  def expr: Parser[Int] = subexpr ~ rep(adder | subtractor) ^^ { case i ~ j => (i /: j)((acc, f) => f(acc)) }
}

trait Solver extends Parser {

  // generate all the expressions
  def generate(nums: Seq[Int]): Seq[String] = {
    @scala.annotation.tailrec
    def recurse(n: Seq[Int], exprs: Seq[String]): Seq[String] = {
      if (n.isEmpty) exprs
      else recurse(n.tail, for (op <- Seq(".", " + ", " - "); expr <- exprs) yield expr + op + n.head)
    }
    recurse(nums.tail, Seq(nums.head.toString))
  }

  // Generates the expressions, evaluates them, and finds those that evaluate to 100, then removes the combine operator.
  def solve(nums: Seq[Int]): Seq[String] =
    generate(nums).filter(c => parseAll(expr, c).get == 100).map(_.replaceAll("\\.", ""))
}

/**
 * Solver ends up generating everything in stages, which makes it kind of hard on RAM particularly if you want
 * to solve harder problem. Wouldn't it be nice to be able to solve this iteratively without requiring everything
 * in ram at once?
 *
 * We can still use the parser, so then the problem becomes to generate every combination of the operators,
 * and then put them in place. This is more or less the same as counting in base 3.
 */
trait Solver2 extends Parser {
  def toOperators(ops: String): String = ops.replaceAll("0", ".").replaceAll("1","+").replaceAll("2","-")

  def generate(op: Int, nums: Seq[Int]): Option[String] = {
    val format = s"%0${nums.size - 1}d"
    val ops = toOperators(String.format(format, Integer.toString(op, 3).toInt.asInstanceOf[java.lang.Integer]))
    if (ops.length == nums.size - 1) {
      val chars: Seq[String] = ops.toSeq.map((c: Char) => c.toString)
      Some(nums.map(_.toString).zipAll(chars, "", "").flatMap(pair => Seq(pair._1, pair._2)).mkString)
    } else None
  }

  def solve(nums: Seq[Int]): Stream[String] = {
    Stream.range(0, math.round(math.pow(3, nums.size - 1)).toInt).map {
      case i: Int => generate(i, nums)
    }.collect {
      case Some(exp) => (exp, parseAll(expr, exp))
    }.filter(_._2.get == 100).map(_._1.replaceAll("\\.", ""))
  }
}

object SolverApp extends App with Solver2 {
  solve(1 to 9).foreach(println)
}
