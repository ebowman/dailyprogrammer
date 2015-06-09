
object GreedyChester extends App {

  val inputs = Seq(
    """
      |6
      |0.9 0.7
      |0.7 0.7
      |0.1 0.1
      |0.4 0.1
      |0.6 0.6
      |0.8 0.8
    """.stripMargin.trim,
  "https://gist.githubusercontent.com/anonymous/5bf6542ebd661804e442/raw/076b6d6dfaf9269f8569b50724efc0ac99013d9b/challenge1.txt",
  "https://gist.githubusercontent.com/anonymous/c06a78cfc6d2cf7e4acf/raw/559686d0aef082c284e1581b36b4541cb87c7934/challenge2.txt",
  "https://gist.githubusercontent.com/anonymous/ed9b5f58dc70910e32e9/raw/7c490275414b0c9cea70aabe4a71c907ef435b25/bonus.txt")

  for (input <- inputs) {
    val source = (if (input.startsWith("https")) io.Source.fromURL(input) else io.Source.fromString(input)).getLines()

    val positions = (1 to source.next().toInt).map { _ =>
      val Array(x, y) = source.next().split("\\s+").map(_.toDouble)
      x -> y
    }

    val startPos = 0.5 -> 0.5

    type Point = (Double, Double)
    def sq(x: Double) = x * x
    def dist(x: Point, y: Point): Double = math.sqrt(sq(x._1 - y._1) + sq(x._2 - y._2))
    def order(x: Point, pos: Seq[Point]): Seq[Point] = pos.map(p => (p, dist(x, p))).sortBy(_._2).map(_._1)

    def mkStream(pos: Point,
                 left: Seq[Point],
                 str: Stream[(Point, Seq[Point])] = Stream.empty): Stream[(Point, Seq[Point])] = {
      if (left.isEmpty) str #::: Stream((pos, left))
      else {
        val next = order(pos, left)
        mkStream(next.head, next.tail, str #::: Stream((pos, left)))
      }
    }

    val start = System.currentTimeMillis()
    val iter = {
      val stream = mkStream(startPos, positions)
      stream.zip(stream.tail).toIterator
    }
    val d = iter.map {
      case ((x, _), (y, _)) => dist(x, y)
    }.sum
    val end = System.currentTimeMillis()
    println(input)
    println(positions.size)
    println(d)
    println(s"${end - start} ms")
  }
}
