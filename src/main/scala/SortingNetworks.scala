
import scala.util.{Failure, Success, Try}

object SortingNetworks extends App {

  case class Wire(var comparators: List[Comparator] = Nil) {
    var result: Option[Int] = None
    private var cursor: List[Comparator] = Nil

    def reset(): Unit = {
      cursor = comparators
      result = None
      comparators.foreach(_.reset())
    }

    def push(i: Int): Unit = {
      if (cursor.isEmpty) result = Some(i)
      else {
        val next = cursor.head
        cursor = cursor.tail
        next.push(i)
      }
    }
  }

  class Router(gt: Wire, lt: Wire) {
    private var primary: Option[Int] = None
    private var secondary: Option[Int] = None

    def reset(): Unit = {
      primary = None
      secondary = None
    }

    def emit(): Unit = {
      require(primary.isDefined)
      require(secondary.isDefined)
      val (Some(p), Some(s)) = (primary, secondary)
      if (p <= s) {
        lt.push(p)
        gt.push(s)
      } else {
        lt.push(s)
        gt.push(p)
      }
    }

    def primary(i: Int): Unit = {
      require(primary.isEmpty)
      primary = Some(i)
      if (secondary.isDefined) emit()
    }

    def secondary(i: Int): Unit = {
      require(secondary.isEmpty)
      secondary = Some(i)
      if (primary.isDefined) emit()
    }
  }

  trait Comparator {
    def push(i: Int): Unit

    def reset(): Unit
  }

  class PrimaryComparator(comparator: Router) extends Comparator {
    def push(i: Int) = comparator.primary(i)

    def reset() = comparator.reset()
  }

  class SecondaryComparator(comparator: Router) extends Comparator {
    def push(i: Int) = comparator.secondary(i)

    def reset() = ()
  }

  val source = io.Source.fromURL(args(0)).getLines()

  val Array(wireCount, comparatorCount) = source.next().split("\\s+").map(_.toInt)
  val wireDesc = source.map(_.split("\\s+").map(_.toInt))
  val wires = for (i <- 1 to wireCount) yield new Wire()

  for (i <- 1 to comparatorCount) {
    val Array(wireLt, wireGt) = source.next().split("\\s+").map(_.toInt)
    val router = new Router(wires(wireGt), wires(wireLt))
    wires(wireLt).comparators = wires(wireLt).comparators :+ new PrimaryComparator(router)
    wires(wireGt).comparators = wires(wireGt).comparators :+ new SecondaryComparator(router)
  }

  Try {
    for (i <- 0 until math.round(math.pow(2, wireCount)).toInt) {
      wires.foreach(_.reset())
      val binary = "0" * (wireCount - i.toBinaryString.length) + i.toBinaryString
      binary.zipWithIndex.foreach { case (digit, index) => wires(index).push(digit - '0') }
      wires.zip(wires.tail).foreach { case (a, b) => assert(a.result.get <= b.result.get) }
    }
  } match {
    case Success(_) => println("Valid Network")
    case Failure(e) if e.isInstanceOf[AssertionError] => println("Invalid Network")
  }
}
