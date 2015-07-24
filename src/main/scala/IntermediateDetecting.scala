/**
 * https://www.reddit.com/r/dailyprogrammer/comments/3e5b0o/20150722_challenge_224_intermediate_detecting/
 */
object IntermediateDetecting extends App {

  //val lineIter = io.Source.fromFile("src/main/resources/IntermediateDetecting.txt").getLines()
  val lineIter = io.Source.fromURL("https://gist.githubusercontent.com/adrian17/48eaa164df394b84a655/raw/5d996d5843e54af05d74f87ae0595ad62d764726/boxes").getLines()

  val data = {
    val d = lineIter.toStream
    val w = d.map(_.length).max
    d.map { line =>
      line + " " * (w - line.length)
    }
  }

  val horizChars = Set('+', '-')
  val vertChars = Set('+', '|')

  def extendDown(x1: Int, x2: Int, y: Int) = {
    val candidates = for {
      row <- data.drop(y).takeWhile(row => vertChars(row(x1)) && vertChars(row(x2)))
    } yield {
      if (row(x1) == row(x2) && row(x2) == '+' && row.substring(x1, x2).forall(horizChars)) 1
      else 0
    }
    candidates.sum
  }

  def extendAcross(x: Int, y: Int) = {
    val candidates = for {
      (cell, nx) <- data(y).drop(x + 1).takeWhile(horizChars).zipWithIndex if cell == '+'
    } yield {
      extendDown(x, x + nx + 1, y + 1)
    }
    candidates.sum
  }

  val n = (for {
    (row, y) <- data.zipWithIndex
    (cell, x) <- row.zipWithIndex if cell == '+'
  } yield extendAcross(x, y)).sum

  println(n)
}
