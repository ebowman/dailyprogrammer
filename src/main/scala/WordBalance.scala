object WordBalance extends App {

  def split(word: String, idx: Int): (String, String) =
    (word.take(idx), word.slice(idx + 1, word.length))

  def weight(str: String, idx: Int, offset: Int): Int =
    str.zipWithIndex.map { case (char, i) => (char - 'A' + 1) * math.abs(i - (idx - offset)) }.sum

  def weigh(word: String, idx: Int): Int = {
    val (prefix, suffix) = split(word, idx)
    val prefixWeight = weight(prefix, idx, 0)
    val suffixWeight = weight(suffix, idx, idx + 1)
    math.max(prefixWeight, suffixWeight) - math.min(prefixWeight, suffixWeight)
  }

  def balance(str: String): String = {
    val result = for (i <- 0 until str.length if weigh(str, i) == 0) yield {
      val (prefix, suffix) = split(str, i)
      s"$prefix ${str(i)} $suffix - ${weight(prefix, i, 0)}"
    }
    if (result.isEmpty) s"$str DOES NOT BALANCE"
    else result.head
  }

  println(balance("STEAD"))
  println(balance("CONSUBSTANTIATION"))
  println(balance("WRONGHEADED"))
  println(balance("UNINTELLIGIBILITY"))
  println(balance("DOESNOTBALANCE"))

  println(balance("ZALANDO"))
  println(balance("ZALANDOTECH"))
  println(balance("HELSINKI"))
  println(balance("PEKKA"))
  println(balance("RADICALAGILITY"))

  val lines = io.Source.fromFile("/usr/share/dict/words").getLines().map(_.toUpperCase)
  lines.filterNot(w =>balance(w).contains("DOES NOT BALANCE")).foreach(w => println((w, balance(w))))
}
