

import scala.math.{pow, sqrt}
import scala.util.parsing.combinator._
import scala.collection.mutable

// https://www.reddit.com/r/dailyprogrammer/comments/3cqxuh/20150710_challenge_222_hard_customer_unit/cszvrqy

case class Location(x: Int, y: Int) {
  @inline def distanceTo(other: Location): Double = sqrt(pow(other.x - x, 2) + pow(other.y - y, 2))
}

case class Depot(truckSize: Int, location: Location)

case class Customer(demand: Int, location: Location)

case class InputData(depot: Depot, customers: Set[Customer])

case class Truck(contents: Int, location: Location)

object InputReader extends JavaTokenParsers {
  def int: Parser[Int] = wholeNumber ^^ (_.toInt)

  def location: Parser[Location] =
    "(" ~ int ~ "," ~ int ~ ")" ^^ { case _ ~ x ~ _ ~ y ~ _ => Location(x, y) }

  def depot: Parser[Depot] =
    int ~ location ^^ { case truckSize ~ loc => Depot(truckSize, loc) }

  def customer: Parser[Customer] =
    int ~ location ^^ { case demand ~ loc => Customer(demand, loc) }

  def customers: Parser[Set[Customer]] =
    rep(customer) ^^ (_.toSet)

  def inputData: Parser[InputData] =
    depot ~ int ~ customers ^^ {
      case depot ~ _ ~ customers => InputData(depot, customers)
    }

  def parse(s: CharSequence): InputData = parse(inputData, s).get
}

object RoutePlanner extends App {
  private def cost(start: Location, stops: Seq[Location]): Double = {
    stops.foldLeft((start, 0.0)) {
      case ((current, cost), stop) => (stop, cost + (current distanceTo stop))
    }._2
  }

  private def deliverToCustomer(truck: Truck,
                                customer: Customer,
                                depot: Depot): (Truck, Seq[Location]) = {
    if (truck.contents >= customer.demand) {
      (Truck(truck.contents - customer.demand, customer.location),
        Seq(customer.location))
    } else {
      (Truck(depot.truckSize - customer.demand, customer.location),
        Seq(depot.location, customer.location))
    }
  }

  private val memo: mutable.Map[(Truck, Set[Customer]), (Truck, Seq[Location])] = {
    import scala.collection.JavaConverters._
    import java.util.concurrent.ConcurrentHashMap
    new ConcurrentHashMap[(Truck, Set[Customer]), (Truck, Seq[Location])]().asScala
  }

  def planRoute2(depot: Depot, truck: Truck, customers: Set[Customer]): (Truck, Seq[Location]) = {
    def actuallyPlan() = customers.par.flatMap { cust =>
      val remaining = customers - cust
      if (remaining.isEmpty) {
        Seq(deliverToCustomer(truck, cust, depot))
      } else {
        val (custTruck, miniRoute) = deliverToCustomer(truck, cust, depot)
        val withoutReturningToDepot = {
          val (endTruck, tailRoute) = planRoute2(depot, custTruck, remaining)
          (endTruck, miniRoute ++ tailRoute)
        }
        val withReturningToDepot = {
          val miniRouteIncludingDepot = miniRoute :+ depot.location
          val (endTruck, tailRoute) =
            planRoute2(depot, Truck(depot.truckSize, depot.location), remaining)
          (endTruck, miniRouteIncludingDepot ++ tailRoute)
        }
        Seq(withoutReturningToDepot, withReturningToDepot)
      }
    }.toSeq.seq.sortBy { case (_, route) => cost(truck.location, route) }.head

    memo.getOrElseUpdate((truck, customers), actuallyPlan())
  }

  val ChallengeInput =
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
    """.stripMargin

  val InputData(depot, customers) = InputReader.parse(ChallengeInput)

  val start = System.currentTimeMillis()
  val (truck, route) =
    RoutePlanner.planRoute2(depot, Truck(depot.truckSize, depot.location), customers)
  val duration = System.currentTimeMillis() - start

  route.foldLeft(depot.location) {
    case (from, to) =>
      println(s"$from -> $to")
      to
  }

  println(s"Total cost: ${cost(depot.location, route)}")
  println(s"Route planned in $duration millis")
  println(memo.size)
}
