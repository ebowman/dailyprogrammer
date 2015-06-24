object SentenceMangling extends App {

  def mangle(str: String): String = {
    val bits = str.split("\\s+")
    if (bits.length > 1) bits.map(mangle).mkString(" ")
    else {
      def upIf(a: Char, b: Char) = if (a.isUpper) b.toUpper else b
      val sorted = bits(0).toLowerCase.filter(_.isLetter).sorted
      bits(0).foldLeft((sorted, "")) {
        case ((srted, running), letter) if letter.isLetter =>
          (srted.tail, running + upIf(letter, srted.head))
        case ((srted, running), punct) =>
          (srted, running + punct)
      }._2
    }
  }

  def test(is: String, shouldBe: String): Unit = assert(is == shouldBe, s"Failed; was [$is] expected [$shouldBe]")

  test(mangle("This challenge doesn't seem so hard."), "Hist aceeghlln denos't eems os adhr.")
  test(mangle("There are more things between heaven and earth, Horatio, than are dreamt of in your philosophy."),
    "Eehrt aer emor ghinst beeentw aeehnv adn aehrt, Ahioort, ahnt aer ademrt fo in oruy hhilooppsy.")
  test(mangle("time-worn"), "eimn-ortw")
  test(mangle("Scrooge McDuck."), "Cegoors CcDkmu.")
  test(mangle("McDonald's hired O.J. O'Connor-McTavish to implement their IoT and iPhone(tm) \"Vélocité\" strategy!"),
  "AcDdlmno's dehir J.O. A'Cchimn-NoOorstv ot eeilmmnpt ehirt IoT adn eHimno(pt) \"Cilotvéé\" aegrstty!")

  Seq("Eye of Newt, and Toe of Frog, Wool of Bat, and Tongue of Dog.",
    "Adder's fork, and Blind-worm's sting, Lizard's leg, and Howlet's wing.",
    "For a charm of powerful trouble, like a hell-broth boil and bubble.").foreach { str =>
    println(str)
    println(mangle(str))
    println()
  }
}
