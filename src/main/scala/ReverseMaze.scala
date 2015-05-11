
/*
http://www.reddit.com/r/dailyprogrammer/comments/34izkl/20150501_challenge_212_hard_reverse_maze/
 */

case class Point(x: Int, y: Int)

object Turn {
  val Left = -1
  val Right = -2
}

object Direction {
  sealed trait Direction {
    def turns: (Direction, Direction)

    def moves: (Int, Int)

    def turn(dir: Int): Direction = dir match {
      case Turn.Right => turns._2
      case Turn.Left => turns._1
    }

    def advance(point: Point): Point = Point(point.x + moves._1, point.y + moves._2)
  }

  object North extends Point(0, 1) with Direction {
    val turns = (West, East)
    val moves = (0, -1)
  }

  object South extends Point(0, -1) with Direction {
    val turns = (East, West)
    val moves = (0, 1)
  }

  object East extends Point(1, 0) with Direction {
    val turns = (North, South)
    val moves = (1, 0)
  }

  object West extends Point(-1, 0) with Direction {
    val turns = (South, North)
    val moves = (-1, 0)
  }

  val all = Seq(North, South, East, West)
}

case class Maze(board: Seq[Seq[Boolean]]) {
  def isClear(point: Point): Boolean = {
    if (point.x < 0 || point.y < 0) false
    else if (point.x >= board.head.size || point.y >= board.size) false
    else board(point.y)(point.x)
  }

  case class Cursor(location: Point, direction: Direction.Direction) {
    def advance(move: Int): Option[Cursor] = move match {
      case m if m >= 0 =>
        val pos = (0 until m).foldLeft(Option(location)) {
          case (Some(loc), _) =>
            val newLoc = direction.advance(loc)
            if (isClear(newLoc)) Some(newLoc)
            else None
          case (None, _) => None
        }
        pos.map(p => this.copy(location = p))
      case dir if dir == Turn.Left || dir == Turn.Right =>
        Some(this.copy(direction = direction.turn(dir)))
    }
  }

}

case class Path(steps: Seq[Int])

object Path {
  def apply(line: String): Path = {
    @scala.annotation.tailrec
    def recurse(line: String, steps: List[Int]): List[Int] = {
      if (line.isEmpty) steps
      else if (line.head == 'r') recurse(line.tail, Turn.Right :: steps)
      else if (line.head == 'l') recurse(line.tail, Turn.Left :: steps)
      else {
        val (next, rest) = line.span(_.isDigit)
        recurse(rest, next.toInt :: steps)
      }
    }
    new Path(recurse(line, Nil).reverse)
  }
}

object Maze {
  def apply(iter: Iterator[String]): Maze =
    new Maze(for (_ <- 1 to iter.next().toInt) yield iter.next().map(c => c.isWhitespace))
}

object ReverseMaze extends App {

  def solve(str: String): Set[String] = {
    val start = System.currentTimeMillis()
    val iterator = io.Source.fromString(str).getLines()

    val maze = Maze(iterator)
    val path = Path(iterator.next())

    val solution = for {
      y <- 0 until maze.board.size
      x <- 0 until maze.board.head.size if maze.isClear(Point(x, y))
      dir <- Direction.all
      start = maze.Cursor(Point(x, y), dir)
    } yield {
        path.steps.foldLeft(Option(start)) {
          case (Some(cur), move) => cur.advance(move)
          case (None, _) => None
        }.map { end =>
          s"From (${start.location.x}, ${start.location.y}) to (${end.location.x}, ${end.location.y})"
        }
      }
    val soln = solution.flatten.toSet
    val stop = System.currentTimeMillis()
    println("Time: " + (stop - start) + "ms")
    soln
  }

  val tests = Seq(
    ( """
        |5
        |xxx
        |x x
        |x x
        |x x
        |xxx
        |2rr2ll2
      """.stripMargin.trim,
      """
        |From (1, 3) to (1, 1)
        |From (1, 1) to (1, 3)
      """.stripMargin.trim.lines.toSet),
    (
      """
        |9
        |xxxxxxxxx
        |x       x
        |xxx x x x
        |x   x x x
        |xxx xxx x
        |x     x x
        |x xxx x x
        |x       x
        |xxxxxxxxx
        |2r2r2
      """.stripMargin.trim,
      """
        |From (3, 7) to (3, 5)
        |From (7, 5) to (5, 5)
        |From (3, 5) to (3, 7)
        |From (5, 3) to (7, 3)
        |From (3, 3) to (5, 3)
        |From (1, 3) to (1, 5)
        |From (1, 1) to (1, 3)
      """.stripMargin.trim.lines.toSet),
    ( """
        |5
        |xxxxxxxxx
        |x   x   x
        |x x x x x
        |x   x   x
        |xxxxxxxxx
        |2r2r2
      """.stripMargin.trim,
      """
        |From (7, 3) to (7, 1)
        |From (5, 3) to (7, 3)
        |From (3, 3) to (3, 1)
        |From (1, 3) to (3, 3)
        |From (7, 1) to (5, 1)
        |From (5, 1) to (5, 3)
        |From (3, 1) to (1, 1)
        |From (1, 1) to (1, 3)
      """.stripMargin.trim.lines.toSet),
    ("""
       |5
       |xxxxxxx
       |x   x x
       |x x x x
       |x x   x
       |xxxxxxx
       |1l2l2
     """.stripMargin.trim,
      """
        |From (3, 2) to (1, 3)
        |From (3, 2) to (5, 1)
      """.stripMargin.trim.lines.toSet)
  )

  for (test <- tests) {
    val result = solve(test._1)
    assert(result == test._2, s"Failed: should be ${test._2}, was $result")
  }

  assert(solve(
    io.Source.fromURL(
      "https://gist.githubusercontent.com/Quackmatic/6119dc82b3cfda54f072/raw/maze-mega.txt"
    ).getLines().mkString("\n")) ==
    """
      |From (1, 9) to (9, 5)
      |From (137, 101) to (145, 97)
      |From (169, 53) to (173, 61)
      |From (211, 121) to (207, 113)
      |From (227, 33) to (219, 37)
    """.stripMargin.trim.lines.toSet)

  assert(solve(
    io.Source.fromURL(
      "https://gist.githubusercontent.com/Quackmatic/7c548fbe4ccff2c08b5f/raw/maze-long.txt"
    ).getLines().mkString("\n")) ==
    io.Source.fromURL(
      "https://gist.githubusercontent.com/Quackmatic/c1361bcebfdd50874f20/raw/maze-long-out.txt"
    ).getLines().toSet
  )
}


