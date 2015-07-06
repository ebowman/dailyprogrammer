
object Go extends App {

  val input = io.Source.fromString(
    """
      |7 5
      |b
      | bbbbb
      |bbwwwwb
      |bww wb
      | bwwwwb
      |  bbbbb
    """.stripMargin.trim).getLines()

  type Pt = (Int, Int)
  type Board = Array[Array[Char]]

  val Array(width, height) = input.next().split(" ").map(_.toInt)
  val me = input.next().head
  def not(c: Char) = if (c == 'b') 'w' else 'b'
  val opponent = not(me)
  val board: Board = (for (_ <- 1 to height) yield {
    val row = input.next().toCharArray
    if (row.size < width) row ++ (" " * (width - row.size)).toCharArray else row
  }).toArray
  assert(!input.hasNext)
  assert(board.head.size == width, board.head.size)
  assert(board.size == height, board.size)

  implicit class TupleOps(val x: Pt) extends AnyVal {
    def +(y: Pt) = (x._1 + y._1, x._2 + y._2)

    def -(y: Pt) = (x._1 - y._1, x._2 - y._2)
  }

  implicit class ArrayOps(val arr: Array[Array[Char]]) {
    def apply(pt: Pt): Char = arr(pt._2)(pt._1)
  }

  def outOfBounds(pt: Pt): Boolean = pt._1 < 0 || pt._1 >= width || pt._2 < 0 || pt._2 >= height

  val directions = Seq((1, 0), (-1, 0), (0, 1), (0, -1))

  def closed(color: Char, pt: Pt, seen: Set[Pt] = Set.empty): Boolean = {
    if (outOfBounds(pt) || seen(pt) || board(pt) == not(color)) true
    else if (board(pt) == ' ') false
    else directions.foldLeft((seen + pt, true)) {
      case ((seen, false), dir) => (seen, false)
      case ((seen, true), dir) => (seen + pt + (pt + dir), closed(color, pt + dir, seen + pt))
    }._2
  }

  for {
    x <- 0 until width
    y <- 0 until height
    pt = (x, y) if board(y)(x) != ' '
  } assert(!closed('b', pt), pt)

  board(2)(3) = 'b'
  println(board.map(_.toSeq.mkString).mkString("\n"))
  //assert(closed('b', (3,2)))
}
