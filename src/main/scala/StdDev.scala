
object StdDev extends App {

  // from https://gist.github.com/oxlade39/5752033
  implicit class Sqrt(val x: BigDecimal) extends AnyVal {
    def sqrt: BigDecimal = {
      val maxIterations = x.mc.getPrecision + 1

      val guessSteam: Stream[BigDecimal] = newtonRaphsonApproximations(x).take(maxIterations)
      val exactMatch: Option[Stream[BigDecimal]] = guessSteam.sliding(2).find(a => a.head == a(1))
      val root: Stream[BigDecimal] = exactMatch.getOrElse(Stream(guessSteam.last))

      root.head
    }

    private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal, guess: BigDecimal): Stream[BigDecimal] =
      Stream.cons(guess, newtonRaphsonApproximations(toSqrt, ((toSqrt / guess) + guess) / 2))

    private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal): Stream[BigDecimal] =
      newtonRaphsonApproximations(toSqrt, toSqrt / 2)
  }

  def sq(x: BigDecimal): BigDecimal = x*x

  implicit class SumSq(val pop: Seq[BigDecimal]) extends AnyVal {
    def sumSq = pop.map(sq).sum
  }

  implicit class StdDev(val pop: Seq[BigDecimal]) extends AnyVal {
    def stddev: BigDecimal = {
      val n = pop.size
      val mean = pop.sum / n
      (pop.map(x => x - mean).sumSq / pop.size).sqrt
    }
  }

  val pop1 = Seq(5, 6, 11, 13, 19, 20, 25, 26, 28, 37).map(BigDecimal.apply)
  assert(f"${pop1.stddev}%.4f" == "9.7775")

  val pop2 = Seq(37, 81, 86, 91, 97, 108, 109, 112, 112, 114, 115, 117, 121, 123, 141).map(BigDecimal.apply)
  assert(f"${pop2.stddev}%.4f" == "23.2908")

  val pop3 = Seq(266, 344, 375, 399, 409, 433, 436, 440, 449, 476, 502, 504, 530, 584, 587).map(BigDecimal.apply)
  println(f"${pop3.stddev}%.4f")

  val pop4 = Seq(809, 816, 833, 849, 851, 961, 976, 1009, 1069, 1125, 1161, 1172, 1178, 1187, 1208, 1215, 1229, 1241, 1260, 1373).map(BigDecimal.apply)
  println(f"${pop4.stddev}%.4f")
}
