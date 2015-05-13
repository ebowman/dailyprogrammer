import java.io.{PrintWriter, FileWriter}


object Animals extends App {

  sealed trait Property {
    def property: String
  }
  case class Is(property: String) extends Property
  case class Has(property: String) extends Property

  case class Node(animals: Set[Animal], has: (Set[Property], Option[Node]), hasNot: (Set[Property], Option[Node]))

  object Node {
    def build(animals: Set[Animal]): Node = {
      val allProps = animals.flatMap(_.properties)
      val first = animals.head
      animals.tail.foldLeft(Node(Set(first), (first.properties, None), (allProps -- first.properties, None))) {
        case (root, animal) =>
         /*

dog
domesticated,a companion,friendly,a mammal,fast
paws,a tail,fur
cat
domesticated,cute,independant,mysterious,a mammal
paws,a tail,fur,claws
            Node(None, ([domesticated,a companion,friendly,a mammal,paws,a tail,fur], dog), (empty, none))
            Node(None, ([domesticated,a mammal, paws,a tail,fur],node1), (empty, none)
              Node(None, ([a companion, friendly, fast], node), (empty, none)
           */
          ???
      }
    }
  }

  /** *
    * {{{
    *   scala> new Animals.Animal("dog", Set(Animals.Is("furry"), Animals.Has("a tail"))).ises
    *   res0: Set[String] = Set(furry)
    *   scala> new Animals.Animal("dog", Set(Animals.Is("furry"), Animals.Has("a tail"))).hases
    *   res0: Set[String] = Set(a tail)
    * }}}
    */
  case class Animal(name: String, properties: Set[Property]) {
    def is(property: String): Boolean = properties.collect {
      case Is(prop) => prop == property
    }.nonEmpty

    def has(property: String): Boolean = properties.collect {
      case Has(prop) => prop == property
    }.nonEmpty

    def isProps = properties.collect { case is@Is(_) => is }
    def hasProps = properties.collect { case has@Has(_) => has }

    def ises = properties.collect {
      case Is(is) => is
    }
    def hases = properties.collect {
      case Has(is) => is
    }

    override def toString =
      s"""
        |$name
        |${ises.mkString(",")}
        |${hases.mkString(",")}
      """.stripMargin.trim
  }

  val animals = io.Source.fromFile("animals.txt").getLines().filterNot(_.trim.isEmpty).grouped(3).map {
    case name :: is :: has :: Nil =>
      Animal(name, is.split(",").toSet.map(Is.apply) ++ has.split(",").map(Has.apply).toSet)
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
    println("What animal were you thinking of?")
    val name = io.StdIn.readLine()
    val exists = animals.find(_.name == name)
    val (newIs, newHas) = (is.isEmpty, has.isEmpty) match {
      case (true, false) =>
        println("What is something that it is?")
        (is + io.StdIn.readLine(), has)
      case (false, true) =>
        println("What is something that it has?")
        (is, has + io.StdIn.readLine())
      case (true, true) =>
        println("What is something that it is?")
        val newIs = is + io.StdIn.readLine()
        println("What is something that it has?")
        val newHas = has + io.StdIn.readLine()
        (newIs, newHas)
      case (false, false) =>
        println("What is something that it is or has? (is XXX or has XXX)")
        io.StdIn.readLine().split("\\s+").toList match {
          case "is" :: prop => (is + prop.mkString(" "), has)
          case "has" :: prop => (is, has + prop.mkString(" "))
          case _ => ???
        }
    }
    save(animals + Animal(name, newIs.map(Is.apply) ++ newHas.map(Has.apply)))
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
      val newAnimal = Animal(animal, is.map(Is.apply) ++ has.map(Has.apply))
      save(animals + newAnimal)
  }
}
