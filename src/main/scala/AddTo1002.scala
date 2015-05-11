import scala.util.parsing.combinator.RegexParsers

/**
 * A parser that handles expressions like "1.2+3" and interprets them like "12+3". It introduces
 * an operator in addition to "+" and "-", the combiner operator ".". The combiner operator has
 * higher precedence than addition and subtraction, and concatenates its arguments together
 * as strings; e.g. 1 . 2 == 12
 */
trait ParserBig extends RegexParsers {
  def number: Parser[BigInt] = "[\\d]+".r ^^ { case n => BigInt(n) }

  def combiner: Parser[BigInt => BigInt] = "." ~> number ^^ { case num => (i: BigInt) => BigInt(s"$i$num") }

  def combined: Parser[BigInt] = number ~ combiner ^^ { case num ~ comb => comb(num) }

  def subexpr: Parser[BigInt] = (combined | number) ~ rep(combiner) ^^ { case x ~ y => (x /: y)((acc, f) => f(acc)) }

  def adder: Parser[BigInt => BigInt] = "+" ~> subexpr ^^ { case sub => (i: BigInt) => i + sub }

  def subtractor: Parser[BigInt => BigInt] = "-" ~> subexpr ^^ { case sub => (i: BigInt) => i - sub }

  def expr: Parser[BigInt] = subexpr ~ rep(adder | subtractor) ^^ { case i ~ j => (i /: j)((acc, f) => f(acc)) }
}

/**
 * Choosing 1..9 is convenient since everything fits in an int. If you go any higher, int isn't enough.
 * I wanted to explore some of the bigger solutions, so I ported Parse to use BigInt (ParserBig), and
 * came up with a scheme so I could iterate through candidates and look for a solution, instead of generating
 * all candidates and searching them (since that uses a lot of memory for larger cases).
 *
 * You can think of something like 1 . 2 + 3 as being represented by a base-3 number of length 2; one digit for
 * each operator. In this case, if . = 0, + = 1 and - = 2, the configuration can be represented by 01. Thus we
 * can map the set of operators applied to a sequence of numbers as just a number, and we can count through them,
 * and then generate the operators for each number. So in this solution, I generate a stream of every number from
 * 0 (so, all '.') up to 22222...2 (so, all '-'), turn each into an expression, evaluate it, and filter for
 * expressions that evaluate to 100. This is fairly efficient in scala using Stream, since it can work incrementally.
 *
 * 1..16 are relatively straightforward. 17 takes some time. for 16 there are 2051 possible expressions; for 17
 * there are 4450.
 */
trait SolverBig extends ParserBig {
  def toOperators(ops: String): String = ops.replaceAll("0", ".").replaceAll("1", "+").replaceAll("2", "-")

  // generate the expression for these nums and this op.
  // so if nums = Seq(1,2,3) and op = 5 (or 12 in base 3), then this returns 1+2-3
  def generate(nums: Seq[Int])(op: BigInt): String = {
    require(op >= 0 && op < math.round(math.pow(3, nums.size - 1)))
    // get a base 3 string rep of the op
    val base = op.toString(3)
    // zero pad it, eg. 2 -> 002 (or ..-)
    val padded = "0" * (nums.size - 1 - base.length) + base
    val ops: Seq[String] = toOperators(padded).toSeq.map(_.toString) // , eg. "012" to Seq(".", "+", "-")
    nums.map(_.toString).zipAll(ops, "", ""). // e.g. Seq("1", ".", "2", "+", "3")
      flatMap(pair => Seq(pair._1, pair._2)).mkString // "1.2+3"
  }

  def solve(nums: Seq[Int]): Stream[String] = {
    val target = BigInt(100)
    // generate every possible sequence of operations, from ..., ..+, ..-, .+., etc., as an int
    val opNumbers = Stream.range(BigInt(0), BigInt(3).pow(nums.size - 1))
    // generate the expressions themselves
    val expressions = opNumbers.map(generate(nums))
    // evaluate each expression
    val pairs = expressions.map(exp => (exp, parseAll(expr, exp)))
    // find the ones that equal the target
    val matches = pairs.filter(_._2.get == target)
    // return just the expressions, with the dots removed
    matches.map(_._1.replaceAll("\\.", ""))
  }
}

object SolverAppBig extends App with SolverBig {
  for (i <- 2 to 17) {
    println((i, solve(1 to i).size))
  }
  solve(1 to 9).foreach(println)
}
