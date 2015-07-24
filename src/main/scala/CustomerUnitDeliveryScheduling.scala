import scala.collection.mutable

object CustomerUnitDeliveryScheduling extends App {

  type Point = (Int, Int)

  @inline def sq(x: Int): Int = x * x

  @inline def distance(a: Point, b: Point): Double = math.sqrt(sq(a._1 - b._1) + sq(a._2 - b._2))

  val input = io.Source.fromString(
    """
      |40 (20,20)
      |12
      |10 (20,8)
      |15 (31,20)
      |18 (13,21)
      |17 (30,20)
      |3 (20,10)
      |5 (11,29)
      |9 (28,12)
      |4 (14,14)
      |6 (32,8)
      |12 (1,1)
      |18 (3,32)
      |23 (5,5)
    """.stripMargin.trim).getLines()

  val RecordFormat = """(\d+) \((\d+),(\d+)\)""".r
  val (capacity, depot) = input.next() match {
    case RecordFormat(cap, x, y) => (cap.toInt, (x.toInt, y.toInt))
  }

  case class Customer(amount: Int, loc: Point)

  object ReturnToBase extends Customer(0, (0,0))
  object DontReturnToBase extends Customer(-1, (0,0))

  val customers = (for (i <- 1 to input.next().toInt) yield {
    input.next() match {
      case RecordFormat(cap, x, y) => Customer(cap.toInt, (x.toInt, y.toInt))
    }
  }).toList

  @scala.annotation.tailrec
  def tourCost(order: List[Customer],
               inTruck: Int = capacity,
               curPos: Point = depot,
               accumCost: Double = 0d): Double = {
    order match {
      case Nil => accumCost
      case ReturnToBase :: tail =>
        tourCost(tail, capacity, depot, accumCost + distance(curPos, depot))
      case DontReturnToBase :: tail => tourCost(tail, inTruck, curPos, accumCost)
      case next :: tail if inTruck >= next.amount =>
        tourCost(tail, inTruck - next.amount, next.loc, accumCost + distance(curPos, next.loc))
      case next :: tail =>
        tourCost(order, capacity, depot, accumCost + distance(curPos, depot))
    }
  }

  def annotate(order: List[Customer]): Set[List[Customer]] = {
    var c = 0
    val size = customers.size
    val max = 1 << size
    val rtnBuffer = new mutable.ListBuffer[List[Customer]]
    val seps = new Array[Customer](size)
    while (c < max) {
      var i = 1
      while (i <= size) {
        if ((c & (1 << i)) == 0) seps(i - 1) = DontReturnToBase
        else seps(i - 1) = ReturnToBase
        i += 1
      }
      rtnBuffer += order.zip(seps).flatMap {
        case (DontReturnToBase, _) => Nil
        case (ReturnToBase, _) => Nil
        case (node, DontReturnToBase) => Seq(node)
        case (node, ReturnToBase) => Seq(node, ReturnToBase)
      }
      c += 1
    }
    rtnBuffer.map(o => if (o.last == ReturnToBase) o.init else o).toSet
  }

  @scala.annotation.tailrec
  def tourDescribe(order: List[Customer], curPos: Point = depot, inTruck: Int = capacity): Unit = {
    order match {
      case Nil =>
      case ReturnToBase :: tail =>
        println(s"Drive ${distance(curPos, depot)} from $curPos back to depot $depot to reload (optimization)")
        tourDescribe(tail, depot, capacity)
      case DontReturnToBase :: tail =>
        tourDescribe(tail, curPos, inTruck)
      case next :: tail =>
        if (inTruck >= next.amount) {
          println(s"Drive ${distance(curPos, next.loc)} from $curPos to ${next.loc}, " +
            s"drop off ${next.amount}, still have ${inTruck - next.amount}")
          tourDescribe(order.tail, next.loc, inTruck - next.amount)
        } else {
          println(s"Drive ${distance(curPos, depot)} from $curPos back to depot $depot to reload")
          tourDescribe(order, depot, capacity)
        }
    }
  }

  val start = System.currentTimeMillis()
  val (best, cost) = customers.permutations.grouped(50).map { orders =>
    orders.par.map { order =>
      annotate(order).map(newOrder => (newOrder, tourCost(newOrder))).minBy(_._2)
    }.minBy(_._2)
  }.minBy(_._2)
  val end = System.currentTimeMillis()
  tourDescribe(best)
  println(s"Total cost = $cost; took ${end - start} ms to solve")
}
