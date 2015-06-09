object TexasHoldem extends App {

  object Suits extends Enumeration {
    type Suit = Value
    val Clovers = Value("♣")
    val Spades = Value("♠")
    val Diamonds = Value("♦")
    val Hearts = Value("♥")
    //val ♣, ♠, ♦, ♥ = Value
  }

  import Suits.Suit

  type Value = String
  val values = Seq("Ace") ++ (2 to 9).map(_.toString) ++ Seq("Jack", "Queen", "King")

  case class Card(suit: Suit, value: Value) {
    override def toString = {
      val text = s"$value $suit"
      "+" + ("-" * (text.length - 2)) + "+" + "\n" +
        "|" + text + "|" + "\n" +
        "+" + ("-" * (text.length - 2)) + "+"
    }

  }

  def deck(): Seq[Card] = {
    val ordered = for {
      suit <- Suits.values
      value <- values
    } yield Card(suit, value)
    util.Random.shuffle(ordered.toSeq)
  }

  println(deck())
}
