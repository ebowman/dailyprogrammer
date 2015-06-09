
import scala.collection.immutable.SortedSet

object Lumberjack extends App {

  for (f <- 1 to 5) {
    val source = io.Source.fromFile(s"src/main/resources/lumberjack/input$f.txt").getLines()
    val boardSize = source.next().trim.toInt
    val logCount = source.next().trim.toInt
    val board = source.flatMap(_.trim.split("\\s+")).map(_.toInt).toIndexedSeq
    val queue = SortedSet(board.zipWithIndex:_*)(new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = if (x._1 == y._1) x._2 - y._2 else x._1 - y._1
    })

    @scala.annotation.tailrec
    def solve(board: SortedSet[(Int, Int)], logs: Int): SortedSet[(Int, Int)] = {
      if (logs == 0) board
      else {
        def incr(x: (Int, Int)) = (x._1 + 1, x._2)
        solve(board.tail + incr(board.head), logs - 1)
      }
    }

    def time[T](f: => T): T = {
      val start = System.currentTimeMillis()
      val result = f
      println(s"${System.currentTimeMillis() - start} ms")
      result
    }

    println(s"input$f.txt")
    val solution = time(solve(queue, logCount))
    println(solution.toSeq.sortBy(_._2).map(_._1).grouped(boardSize).map(_.mkString(" ")).mkString("\n"))
  }
}
