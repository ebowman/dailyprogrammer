

object SadCycles extends App {

  def sad(power: Int, start: BigDecimal): Seq[BigDecimal] = {

    def next(n: BigDecimal): BigDecimal =  n.toString.map(i => BigDecimal(i.toString).pow(power)).sum

    @scala.annotation.tailrec
    def findRepeats(nums: Seq[BigDecimal]): Seq[BigDecimal] = {
      val n = next(nums.head)
      if (nums.contains(n)) nums.take(nums.indexOf(n) + 1)
      else findRepeats(n +: nums)
    }

    findRepeats(List(start)).reverse
  }

  println(sad(5, 117649).mkString(" "))
  println(sad(6, 2).mkString(" "))
  println(sad(7, 7).mkString(" "))
  println(sad(3, 14).mkString(" "))
  println(sad(11, 2).mkString(" "))
}
