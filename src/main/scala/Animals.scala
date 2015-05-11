import java.io.{PrintWriter, FileWriter}

object Animals extends App {

  case class Animal(name: String, ises: Set[String], hases: Set[String]) {
    def is(property: String): Boolean = ises.contains(property)

    def has(property: String): Boolean = hases.contains(property)

    override def toString =
      s"""
        |$name
        |${ises.mkString(",")}
        |${hases.mkString(",")}
      """.stripMargin.trim
  }

  val animals = io.Source.fromFile("animals.txt").getLines().filterNot(_.trim.isEmpty).grouped(3).map {
    case name :: is :: has :: Nil =>
      Animal(name, is.split(",").toSet, has.split(",").toSet)
    case huh => println(huh.mkString("|")); sys.error("")
  }.toSet

  util.Random.setSeed(System.currentTimeMillis())

  @scala.annotation.tailrec
  def solve(remainingAnimals: Set[Animal], knownIs: Set[String], knownHas: Set[String]): (Option[String], Set[String], Set[String]) = {
    if (remainingAnimals.isEmpty) (None, knownIs, knownHas)
    else if (remainingAnimals.size == 1) (remainingAnimals.headOption.map(_.name), knownIs, knownHas)
    else {
      import util.Random.shuffle
      val remainingIses = remainingAnimals.flatMap(_.ises).filterNot(p => remainingAnimals.forall(_.ises.contains(p)))
      val remainingHaves = remainingAnimals.flatMap(_.hases).filterNot(p => remainingAnimals.forall(_.hases.contains(p)))
      val questionIs = util.Random.nextBoolean()
      val prop = if (questionIs) shuffle(remainingIses.toSeq).head else shuffle(remainingHaves.toSeq).head
      val randomQuestion =
        if (questionIs) s"Is the animal $prop (yes or no)" else s"Does the animal have $prop (yes or no)"
      println(randomQuestion)
      val (newAnimals, newIses, newHas) = io.StdIn.readLine() match {
        case "yes" =>
          if (questionIs) (remainingAnimals.filter(_.ises.contains(prop)), knownIs + prop, knownHas)
          else (remainingAnimals.filter(_.hases.contains(prop)), knownIs, knownHas + prop)
        case "no" =>
          if (questionIs) (remainingAnimals.filterNot(_.ises.contains(prop)), knownIs, knownHas)
          else (remainingAnimals.filterNot(_.hases.contains(prop)), knownIs, knownHas)
      }
      solve(newAnimals, newIses, newHas)
    }
  }

  def save(animals: Set[Animal]): Unit = {
    val writer = new PrintWriter(new FileWriter("animals.txt"))
    try writer.println(animals.map(_.toString).mkString("\n\n"))
    finally writer.close()
  }

  def updateLibrary(is: Set[String], has: Set[String]): Unit = {
    ???
//    println("What animal were you thinking of?")
//    val name = io.StdIn.readLine()
//    val existed
//    val (newIs, newHas) = (is.isEmpty, has.isEmpty) match {
//      case (true, false) =>
//        println("What is something that it is?")
//        (is + io.StdIn.readLine(), has)
//      case (false, true) =>
//        println("What is something that it has?")
//        (is, has + io.StdIn.readLine())
//      case (true, true) =>
//        println("What is something that it is?")
//        val newIs = is + io.StdIn.readLine()
//        println("What is something that it has?")
//        val newHas = has + io.StdIn.readLine()
//        (newIs, newHas)
//      case (false, false) =>
//        println("What is something that it is or has? (is XXX or has XXX)")
//        io.StdIn.readLine().split("\\s+").toList match {
//          case "is" :: prop => (is + prop.mkString(" "), has)
//          case "has" :: prop => (is, has + prop.mkString(" "))
//        }
//    }
//    save(animals + Animal(name, newIs, newHas))
  }

  solve(animals, Set.empty, Set.empty) match {
    case (Some(animal), is, has) =>
      println(s"I think the animal is a $animal")
      println("Was I right? (yes or no) ")
      io.StdIn.readLine() match {
        case "yes" => println("Thanks for playing!")
        case "no" => updateLibrary(is, has)
      }
    case (None, is, has) =>
      println(s"I don't know what the animal is")
      println("What was it?")
      val animal = io.StdIn.readLine()
      val newAnimal = Animal(animal, is, has)
      save(animals + newAnimal)
  }
}
