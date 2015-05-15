import java.util
import java.util.concurrent.atomic.AtomicLong

object PileOfPaperOn2 extends App {
  //  val iter = io.Source.fromString(
  //    """
  //      |20 10
  //      |1 5 5 10 3
  //      |2 0 0 7 7
  //    """.stripMargin.trim).getLines()
  //  println(solve(iter))
  println(solve(io.Source.fromFile("src/main/resources/piles/100rects100x100.in").getLines()))

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

object PileOfPaperOn extends App {

  case class Board(width: Int, height: Int)

  case class Sheet(color: Int, width: Int, height: Int, x: Int, y: Int) {
    def overlay(y: Int, row: Array[Int]): Array[Int] = {
      if (y >= this.y && y < this.y + height) util.Arrays.fill(row, x, x + width, color)
      row
    }
  }

  def solve(iter: Iterator[String]): Seq[String] = {

    val board = {
      val Array(width, height) = iter.next().split("\\s+").map(_.toInt)
      Board(width, height)
    }

    val sheets = iter.flatMap {
      line => line.split("\\s+").map(_.toInt).grouped(5).map {
        case Array(color, width, height, x, y) => Sheet(color, x, y, width, height)
      }
    }.toList

    val working = new Array[Int](board.width)
    val solution: Array[Int] = (0 until board.height).foldLeft(Array.fill(sheets.map(_.color).max + 1)(0)) {
      case (counts, y) =>
        if ((y % 1000) == 0) println(y)
        util.Arrays.fill(working, 0)
        @scala.annotation.tailrec
        def overlay(sheets: List[Sheet]): Unit =
          if (sheets.isEmpty) ()
          else {
            sheets.head.overlay(y, working);
            overlay(sheets.tail)
          }
        overlay(sheets)
        working.foreach(color => counts(color) += 1)
        counts
    }

    assert(solution.sum == board.width * board.height)
    solution.zipWithIndex.flatMap {
      case (0, idx) => None
      case (count, idx) => Some(s"$idx $count")
    }
  }

  assert(solve(
    io.Source.fromString(
      """
        |20 10
        |1 5 5 10 3
        |2 0 0 7 7
      """.stripMargin.trim
    ).getLines()) == Seq("0 125", "1 26", "2 49"))

  val ls = {
    import scala.sys.process._
    "ls src/main/resources/piles/".!!
  }
  for {
    pair <- ls.lines.toSeq.sorted.grouped(2).toSeq.reverse if pair.size == 2
  } {
    println(pair)
    val start = System.currentTimeMillis()
    val solution: Seq[String] = solve(io.Source.fromFile(s"src/main/resources/piles/${pair.head}").getLines())
    val end = System.currentTimeMillis()
    println(s"${end - start} ms")
    val shouldBe: Seq[String] = io.Source.fromFile(s"src/main/resources/piles/${pair.tail.head}").getLines().toIndexedSeq
    assert(solution == shouldBe, s"was: $solution shouldBe: $shouldBe")
  }
}


object PileOfPaper2On extends App {

  case class Board(width: Int, height: Int)

  case class Sheet(color: Int, width: Int, height: Int, x: Int, y: Int) {
    @inline def intersects(x: Int, y: Int): Boolean = x >= this.x && x < this.x + width && y >= this.y && y < this.y + height
  }

  def solve(iter: Iterator[String]): Seq[(Long, Int)] = {

    val board = {
      val Array(width, height) = iter.next().split("\\s+").map(_.toInt)
      Board(width, height)
    }

    val sheets = iter.flatMap {
      line => line.split("\\s+").map(_.toInt).grouped(5).map {
        case Array(color, width, height, x, y) => Sheet(color, x, y, width, height)
      }
    }.toSeq.reverse


    val blackboard = Array.fill(sheets.map(_.color).max + 1)(new AtomicLong)

    val groupSize = 10000
    (0 until board.height) foreach { y =>
      if ((y % 10000) == 0) println(y)
      (0 until board.width).grouped(groupSize).foreach { case group =>
        group.par.map { case x =>
          sheets.find(_.intersects(x, y)) match {
            case None => blackboard(0).incrementAndGet()
            case Some(sheet) => blackboard(sheet.color).incrementAndGet()
          }
        }
      }
    }

    blackboard.map(_.get).zipWithIndex.filterNot(_._1 == 0)
  }

  val ls = {
    import scala.sys.process._
    "ls src/main/resources/piles/".lineStream.filter(_.endsWith(".in")) //.filter(_.contains("100x100"))
  }

  def lines(f: String): Iterator[String] = io.Source.fromFile(s"src/main/resources/piles/$f").getLines()

  for (file <- ls) {
    println(file)
    val start = System.currentTimeMillis()
    val solution: Seq[(Long, Int)] = solve(lines(file))
    val end = System.currentTimeMillis()
    println(s"${end - start} ms")
    println(solution.map {
      case (count, color) => s"$color $count"
    }.mkString("\n"))
    println(s"sum = ${solution.map(_._1).sum}")
  }
}
