

object Algorithms {
  def bruteForce(numbers: Seq[Int]): BigInt = {
    numbers.map(_.toString).permutations.map {
      case pieces: Seq[String] => BigInt(pieces.mkString)
    }.max
  }

  def sorting(numbers: Seq[Int]): BigInt = {
    BigInt(numbers.sortWith {
      case (a, b) => (a.toString + b.toString) > (b.toString + a.toString)
    }.mkString)
  }

  def broken(numbers: Seq[Int]): BigInt = {
    BigInt {
      numbers.map(_.toString).sortWith {
        case (a, b) =>
          val len = math.max(a.length, b.length)
          val A = a + "0" * (len - a.length)
          val B = b + "0" * (len - a.length)
          A > B
      }.mkString
    }
  }
}
