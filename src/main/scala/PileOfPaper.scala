
case class Board(tiles: Seq[Seq[Int]]) {
  def layer(sheet: Sheet): Board = {
    Board(for {
      y <- 0 until tiles.size
      row = tiles(y)
    } yield {
        if (y < sheet.y || y >= sheet.y + sheet.height) row
        else update(row, sheet.x, sheet.width, sheet.color)
      })
  }

  private def update(row: Seq[Int], col: Int, width: Int, color: Int): Seq[Int] = {
    @scala.annotation.tailrec
    def recurse(row: Seq[Int], x: Int, color: Int): Seq[Int] = {
      if (x == 0) row.updated(col + x, color)
      else recurse(row.updated(col + x, color), x - 1, color)
    }
    recurse(row, width - 1, color)
  }

  def count(color: Int): Int = {
    (for {row <- tiles} yield row.count(_ == color)).sum
  }

  override def toString = tiles.map(_.mkString(" ")).mkString("\n")
}

object Board {
  def apply(line: String): Board = {
    val Array(columns, rows) = line.split("\\s+").map(_.toInt).take(2)
    new Board(for {y <- 0 until rows} yield Seq.fill(columns)(0))
  }
}

case class Sheet(color: Int, x: Int, y: Int, width: Int, height: Int) {
  override def toString = s"Sheet(color=$color, x=$x, y=$x, width=$width, height=$height)"
}

object Sheet {
  def apply(line: String): Sheet = {
    val Array(color, x, y, width, height) = line.split("\\s+").map(_.toInt).take(5)
    new Sheet(color, x, y, width, height)
  }
}

object PileOfPaper extends App {
  val iter = io.Source.fromString(
    """
      |20 10
      |1 5 5 10 3
      |2 0 0 7 7
    """.stripMargin.trim).getLines()
  println(solve(iter))

  val ls = {
    import scala.sys.process._
    "ls src/main/resources/piles/".!!
  }
  for {
    pair <- ls.lines.toIndexedSeq.grouped(2) if pair.size == 2
  } {
    val solution = solve(io.Source.fromFile(s"src/main/resources/piles/${pair.head}").getLines())
    val shouldBe = io.Source.fromFile(s"src/main/resources/piles/${pair.tail.head}").mkString("\n")
    println(solution)
    assert(solution == shouldBe, s"was: $solution shouldBe: $shouldBe")
  }

  def solve(iter: Iterator[String]): String = {
    val board = Board(iter.next())
    val sheets = (for (line <- iter) yield Sheet(line)).toSeq

    val solution = sheets.foldLeft(board) {
      case (b, s) => b.layer(s)
    }

    val colors = for {
      color <- 0 +: sheets.map(_.color)
    } yield color -> solution.count(color)
    colors.toIndexedSeq.sortBy(_._1).map(p => s"${p._1} ${p._2}").mkString("\n")
  }
}
