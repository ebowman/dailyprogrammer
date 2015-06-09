
object TetrisLamp extends App {

  val input = "rats live on arona no evil star"

  val reverseTable = ('a' to 'z').reverse

  @scala.annotation.tailrec
  def addOne(input: Seq[Int]): Seq[Int] = {
    if (input.max == 99) input
    else addOne(input.map(_ + 1))
  }

  println(
    addOne(
      input.map(
        c => reverseTable.lift(c - 'a').getOrElse(c) - 'a' + 1)
    ).reverse.map(c => (100 - c + 'a' - 1).toChar).mkString)
}
