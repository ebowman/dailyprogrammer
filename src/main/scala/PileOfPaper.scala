import java.util
import java.util.concurrent.{CountDownLatch, Executors}

import scala.language.implicitConversions

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

  final case class Sheet(color: Int, width: Int, height: Int, x: Int, y: Int) {
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
    }.toSeq.reverse.toArray


    val threadCount = 5
    val blackboards = Array.fill(threadCount)(Array.fill(sheets.map(_.color).max + 1)(0L))
    val executor = Executors.newFixedThreadPool(threadCount)

    implicit def f2r(fun: => Unit): Runnable = new Runnable {
      def run() {
        fun
      }
    }

    val latch = new CountDownLatch(threadCount)
    for (block <- 0 until threadCount) {
      executor.submit {
        var y = block * board.height / threadCount
        val rowMax = (block + 1) * board.height / threadCount
        val blackboard = blackboards(block)
        while (y < rowMax) {
          var x = 0
          while (x < board.width) {
            // if ((x % 10000) == 0 && (y % 1000) == 0) println((x, y))
            var (i, done) = (0, false)
            val sheetCount = sheets.length
            while (!done && i < sheetCount) {
              val curSheet = sheets(i)
              if (curSheet.intersects(x, y)) {
                blackboard(curSheet.color) += 1
                done = true
              }
              i += 1
            }
            if (!done) blackboard(0) += 1
            x += 1
          }
          y += 1
        }
        latch.countDown()
      }
    }
    executor.shutdown()
    latch.await()
    (for (i <- 0 until sheets.map(_.color).max + 1) yield {
      blackboards.map(b => b(i)).sum
    }).zipWithIndex.filterNot(_._1 == 0)
  }

  val ls = {
    import scala.sys.process._
    "ls src/main/resources/piles/".lineStream.toSeq.filter(_.endsWith(".in")) //.filter(_.contains("10Kx10K"))
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

object PileOfPaperFast extends App {

  object Rect {
    val Empty = Rect(0, 0, 0, 0, 0)
  }

  case class Rect(color: Int, x: Int, y: Int, width: Int, height: Int) {
    def isEmpty = width <= 0 || height <= 0

    def contains(x: Int, y: Int) = x >= this.x && y >= this.y && x < this.x + width && y < this.y + height

    def area = width * height.toLong

    def clip(r: Rect): Rect = {
      if (r.x + r.width <= x) Rect.Empty
      else if (r.x >= x + width) Rect.Empty
      else if (r.y + r.height <= y) Rect.Empty
      else if (r.y >= y + height) Rect.Empty
      else {
        val newX = math.max(x, r.x)
        val newY = math.max(y, r.y)
        val newWidth = math.min(r.width, math.min(x + width, r.x + r.width) - newX)
        val newHeight = math.min(r.height, math.min(y + height, r.y + r.height) - newY)
        r.copy(x = newX, y = newY, width = newWidth, height = newHeight)
      }
    }

    def cover(rect: Rect): Set[Rect] = {
      val isect = {
        val r = clip(rect)
        if (x == r.x && y == r.y && width == r.width && height == r.height) Set.empty[Rect]
        else if (rect.x + rect.width <= x) Set(this, rect)
        else if (rect.x >= x + width) Set(this, rect)
        else if (rect.y + rect.height <= y) Set(this, rect)
        else if (rect.y >= y + height) Set(this, rect)
        else if (clip(rect).isEmpty) Set(this, rect)
        else {
          (r.y - y, r.x - x, x + width - (r.x + r.width), y + height - (r.y + r.height)) match {
            case (0, 0, 0, heightBottom) =>
              Set(Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (0, 0, widthRight, 0) =>
              Set(Rect(color, x + width - widthRight, y, widthRight, height))
            case (0, widthLeft, 0, 0) =>
              Set(Rect(color, x, y, widthLeft, height))
            case (heightTop, 0, 0, 0) =>
              Set(Rect(color, x, y, width, heightTop))
            case (0, widthLeft, 0, heightBottom) =>
              Set(Rect(color, x, y, widthLeft, height - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (heightTop, 0, widthRight, 0) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x + width - widthRight, y + heightTop, widthRight, height - heightTop))
            case (0, 0, widthRight, heightBottom) =>
              Set(Rect(color, x + width - widthRight, y, widthRight, height - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (heightTop, widthLeft, 0, 0) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x, y + heightTop, widthLeft, height - heightTop))
            case (0, widthLeft, widthRight, 0) =>
              Set(Rect(color, x, y, widthLeft, height),
                Rect(color, x + width - widthRight, y, widthRight, height))
            case (heightTop, 0, 0, heightBottom) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (0, widthLeft, widthRight, heightBottom) =>
              Set(Rect(color, x, y, widthLeft, height - heightBottom),
                Rect(color, x + width - widthRight, y, widthRight, height - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (heightTop, 0, widthRight, heightBottom) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x + width - widthRight, y + heightTop, widthRight, height - heightTop - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (heightTop, widthLeft, 0, heightBottom) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x, y + heightTop, widthLeft, height - heightTop - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
            case (heightTop, widthLeft, widthRight, 0) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x, y + heightTop, widthLeft, height - heightTop),
                Rect(color, x + width - widthRight, y + heightTop, widthRight, height - heightTop))
            case (heightTop, widthLeft, widthRight, heightBottom) =>
              Set(Rect(color, x, y, width, heightTop),
                Rect(color, x, y + heightTop, widthLeft, height - heightTop - heightBottom),
                Rect(color, x + width - widthRight, y + heightTop, widthRight, height - heightTop - heightBottom),
                Rect(color, x, y + height - heightBottom, width, heightBottom))
          }
        }
      }
      isect.map(clip).filterNot(_.isEmpty)
    }
  }

  val files = {
    import scala.sys.process._
    "ls src/main/resources/piles/".!!
  }
  for (file <- files.lines.toSeq.filter(_ endsWith ".in")) {
    println(file)
    val source = io.Source.fromFile(s"src/main/resources/piles/$file").getLines()
    val background = {
      val line = source.next().split("\\s+").map(_.toInt)
      Rect(0, 0, 0, line(0), line(1))
    }
    val sheets = background :: source.flatMap {
      line => line.split("\\s+").map(_.toInt).grouped(5).map {
        case Array(color, x, y, width, height) => Rect(color, x, y, width, height)
      }
    }.toList

    @scala.annotation.tailrec
    def solve(visible: Seq[Rect], sheets: List[Rect]): Seq[Rect] = {
      if (sheets.isEmpty) visible
      else solve(visible.flatMap(_.cover(sheets.head)) :+ sheets.head, sheets.tail)
    }

    def time[T](f: =>T): T = {
      val start = System.currentTimeMillis()
      val result = f
      println(s"${System.currentTimeMillis() - start} ms")
      result
    }

    val solution = time(solve(Seq(sheets.head), sheets.tail))
    val groups = solution.groupBy(_.color)

    println(groups.mapValues(_.map(_.area).sum).toSeq.sortBy(_._1).map(cc => s"${cc._1} ${cc._2}").mkString("\n"))
    println()
  }
}
