// run this app passing in the path to an input file, prints out the total time taken
object Elevators extends App {

  // describes the properties of a car
  case class CarDesc(name: String, capacity: Int, secondsPerFloor: Int, position: Int)

  object CarDesc {
    // generates a CarDesc from a line in the input file
    def apply(record: String): CarDesc = {
      val bits = record.split("\\s+")
      new CarDesc(bits(0), bits(1).toInt, math.round(1f / bits(2).toFloat), bits(3).toInt)
    }
  }

  // describes a journey
  case class Journey(name: String, initialTime: Int, start: Int, end: Int)

  object Journey {
    // generates a Journey instance from a line in the input file
    def apply(record: String): Journey = {
      val bits = record.split("\\s+")
      new Journey(bits(0), bits(1).toInt, bits(2).toInt, bits(3).toInt)
    }

    val NullJourney = Journey("", 0, 0, 0)
  }

  // represents the direction an elevator car is travelling
  object Direction extends Enumeration {
    type Direction = Value
    val Up = Value(1)
    val Down = Value(-1)

    def dir(int: Int): Direction = if (int < 0) Down else Up
  }

  import Direction._

  // represents a distinct state for an elevator car: its properties, its current floor, which
  // direction it's travelling, what time it thinks it is, and who is one it.
  case class Car(desc: CarDesc, curPos: Int, curDir: Option[Direction], curTime: Int, riders: Seq[Journey]) {
    def nextFloor = this.copy(curPos = curPos + curDir.map(_.id).getOrElse(0), curTime = curTime + desc.secondsPerFloor)
  }

  val source = io.Source.fromFile(args(0)).getLines()

  // load all the cars
  val cars = for (_ <- 1 to source.next().toInt) yield {
    val desc = CarDesc(source.next())
    Car(desc, desc.position, None, 0, Nil)
  }

  // load all the journeys
  val journeys = for (_ <- 1 to source.next().toInt) yield Journey(source.next())

  // run the simulation
  simulate(cars, journeys, 0)

  @scala.annotation.tailrec
  def simulate(cars: Seq[Car], journeys: Seq[Journey], clock: Int): Unit = {

    // set of riders available and waiting
    val waitingRiders: Set[Journey] = journeys.filter(_.initialTime <= clock).toSet

    // make sure the cars are started if anybody is waiting
    val startedCars: Seq[Car] = cars.zipAll(waitingRiders.take(cars.size), cars.head, Journey.NullJourney).map {
      case (car, rider) =>
        if (car.curDir.isEmpty) car.copy(curDir = Some(dir(rider.start - car.curPos)))
        else car
    }

    // floors currently with people waiting for an elevator, or that we are en route to drop off
    val waitingFloors: Seq[Int] = (waitingRiders.map(_.start) ++ cars.flatMap(_.riders.map(_.end))).toSeq.sorted

    // make sure that any car that has nothing more to do in its current direction, switches direction
    val convertedCars = startedCars.map { car =>
      if (waitingFloors.isEmpty) car
      else {
        def bounds[T](seq: Seq[T]): (T, T) = (seq.head, seq.last)
        val (min, max) = bounds(waitingFloors)
        if (car.curPos > max && car.curDir.contains(Up)) car.copy(curDir = Some(Down))
        else if (car.curPos < min && car.curDir.contains(Down)) car.copy(curDir = Some(Up))
        else car
      }
    }

    // find all passengers getting off at this floor, and update cars to remove these passengers
    val unloadedCars = convertedCars.foldLeft(Seq.empty[Car]) {
      case (newCars, car) =>
        newCars :+ car.copy(riders = car.riders.filterNot(_.end == car.curPos))
    }

    // load any passengers waiting on a floor where there is an elevator available
    val (_, gotOn, loadedCars) = unloadedCars.foldLeft((waitingRiders, Set.empty[Journey], Seq.empty[Car])) {
      case ((waiting, gettingOn, newCars), car) =>
        val available = waiting.filter(j => j.start == car.curPos && j.initialTime <= clock)
        (waiting -- available, gettingOn ++ available, newCars :+ car.copy(riders = car.riders ++ available))
    }

    // advance all elevators to their next floor
    val newCarPositions: Seq[Car] = loadedCars.map(_.nextFloor)

    // if we are done, stop recursing
    if (newCarPositions.forall(_.riders.isEmpty) && journeys.forall(gotOn)) {
      println(s"Total time: $clock")
    } else {
      // copute the new time and recurse
      val curTime = newCarPositions.map(_.curTime).min
      simulate(newCarPositions, journeys.filterNot(gotOn), curTime)
    }
  }
}
