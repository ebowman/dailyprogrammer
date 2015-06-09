import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.util.{Failure, Success, Try}

object TexasHoldemActor extends App {

  sealed trait Suit

  case object Hearts extends Suit

  case object Diamonds extends Suit

  case object Spades extends Suit

  case object Clubs extends Suit

  type Value = String
  val values = Seq("Ace") ++ (2 to 9).map(_.toString) ++ Seq("Jack", "Queen", "King")

  case class Card(suit: Suit, value: Value)

  case object ShowCards

  def deck(): Seq[Card] = {
    val ordered = for {
      suit <- Seq(Hearts, Diamonds, Spades, Clubs)
      value <- values
    } yield Card(suit, value)
    util.Random.shuffle(ordered)
  }

  object Player {
    def props(name: String): Props = Props(classOf[Player], name)
  }

  class Player(name: String) extends Actor {
    var cards: Seq[Card] = Nil

    override def receive: Receive = {
      case card: Card => cards = cards :+ card
      case ShowCards =>
        println(name + "\n" + cards.mkString("\n"))
    }
  }

  object Dealer {

    case object Begin

    def props(players: Int): Props = Props(classOf[Dealer], players)
  }

  class Dealer(pCount: Int) extends Actor {
    var players: Seq[ActorRef] = Nil
    var community: ActorRef = null
    var cards: Seq[Card] = Nil

    override def receive: Receive = {
      case Dealer.Begin =>
        cards = deck()
        players = for (n <- 1 to pCount) yield system.actorOf(Player.props(s"player $n"))
        community = system.actorOf(Player.props(s"community"))
        nextHand()
    }

    def nextHand(): Unit = {
      players.foreach { player =>
        cards.take(2).foreach(card => player ! card)
        cards = cards.drop(2)
      }
      cards = cards.drop(1)
      cards.take(5).foreach(card => community ! card)
      cards = cards.drop(5)
      (players :+ community).foreach(_ ! ShowCards)
    }
  }

  val system = ActorSystem("TexasHoldem")

  def playerCount(): Int = {
    println("How many players (2-8) ? ")
    Try {
      io.StdIn.readLine() match {
        case ok if ok.toInt >= 2 && ok.toInt <= 8 => ok.toInt
        case _ => playerCount()
      }
    } match {
      case Success(count) => count
      case Failure(_) => playerCount()
    }
  }

  val count = playerCount()
  val dealer = system.actorOf(Dealer.props(count))
  dealer ! Dealer.Begin
  Thread.sleep(100000)
  system.shutdown()
}
