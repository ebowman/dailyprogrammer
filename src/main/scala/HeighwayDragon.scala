import java.io.{File, FileWriter, PrintWriter}

object HeighwayDragon extends App {

  type Pt = (Int, Int)

  def heighwayStream: Stream[Pt] = {

    def next(in: Stream[(Pt, Int)]): Stream[(Pt, Int)] = {
      def turn(n: Int) = (((n & (n * -1)) << 1) & n) != 0
      def nextTurn(x: Pt, y: Pt, n: Int) = if (turn(n)) turnRight(x, y) else turnLeft(x, y)
      val Stream(p0, p1) = in.takeRight(2).unzip._1
      val lastIdx = in.last._2
      val p2 = nextTurn(p0, p1, lastIdx)
      val p3 = nextTurn(p1, p2, lastIdx + 1)
      val p4 = nextTurn(p2, p3, lastIdx + 2)
      in #::: next(Stream(p2, p3, p4).zipWithIndex.map(vi => (vi._1, vi._2 + lastIdx + 1)))
    }

    def turnRight(p0: Pt, p1: Pt): Pt =
      (p1._1 - p0._1, p1._2 - p0._2) match {
        case (1, 0) => (p1._1, p1._2 - 1)
        case (-1, 0) => (p1._1, p1._2 + 1)
        case (0, 1) => (p1._1 + 1, p1._2)
        case (0, -1) => (p1._1 - 1, p1._2)
      }

    def turnLeft(p0: Pt, p1: Pt): Pt =
      (p1._1 - p0._1, p1._2 - p0._2) match {
        case (1, 0) => (p1._1, p1._2 + 1)
        case (-1, 0) => (p1._1, p1._2 - 1)
        case (0, 1) => (p1._1 - 1, p1._2)
        case (0, -1) => (p1._1 + 1, p1._2)
      }

    next(Stream((0, 0), (1, 0)).zipWithIndex).map(_._1)
  }

  def heighway(n: Int) = {
    require(math.round(math.pow(2, n)) < Int.MaxValue, s"${math.round(math.pow(2, n))} >= ${Int.MaxValue}")
    heighwayStream.take(math.round(math.pow(2, n)).toInt + 1)
  }

  val (x12, y12) = heighway(12).unzip

  assert(x12.sum == -104896)
  assert(y12.sum == 52416)

  val blocks = (BigInt("2").pow(100) + 1) / Int.MaxValue
  println(blocks)
  val (x100, y100) = heighway(100).unzip
  val s = System.currentTimeMillis()
  println(x100.par.sum)
  println(s"${System.currentTimeMillis() - s}")
  val t = System.currentTimeMillis()
  println(y100.par.sum)
  println(s"${System.currentTimeMillis() - t}")


  import scala.sys.process._
  val w = new PrintWriter(new FileWriter("plot.gnu"))
  val format = "postscript"
  val suffix = "ps"
  w.println(s"""
    |set terminal $format
    |set output ofilename
    |unset border
    |unset xtics
    |unset ytics
    |unset key
    |set size ratio -1
    |plot filename with lines
  """.stripMargin)
  w.close()
  for (n <- (1 to 25).par) {
    val tmp = File.createTempFile(".gnu", ".dat")
    tmp.deleteOnExit()
    val writer = new PrintWriter(new FileWriter(tmp))
    heighway(n).foreach(x => writer.println(s"${x._1} ${x._2}"))
    writer.close()
    Seq("gnuplot", "-e", s"filename='${tmp.getAbsolutePath}';ofilename='heightway-$n.$suffix'", "plot.gnu").!
  }


//  val n = 25
//  val start = System.currentTimeMillis()
//  heighway(n).last
//  val end = System.currentTimeMillis()
//  println(s"n = $n took ${end - start} ms")
}
