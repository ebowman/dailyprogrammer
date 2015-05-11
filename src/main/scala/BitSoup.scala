object BitSoup extends App {

  val words = Map(0 -> "zero", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six",
    7 -> "seven", 8 -> "eight", 9 -> "nine", 0xA -> "ehh", 0xB -> "bee", 0xC -> "cee", 0xD -> "dee", 0xE -> "eee",
    0xF -> "eff", 0xA0 -> "atta", 0xB0 -> "bibbity", 0xC0 -> "city", 0xD0 -> "dickety", 0xE0 -> "ebbity", 0xF0 -> "fleventy")

  def word(w: Int) = (w & 0xF0, w & 0x0F) match {
    case (high, 0) => words(high)
    case (high, low) => s"${words(high)}-${words(low)}"
  }

  def pronounce(x: Int) = (x >> 8, x & 0xFF) match {
    case (0, low) => word(low)
    case (high, 0) => s"${word(high)} bitey"
    case (high, low) => s"${word(high)} bitey ${word(low)}"
  }

  def check(num: Int, pronunciation: String): Unit = {
    def hex(i: Int) = s"0x${i.toHexString.toUpperCase}"
    println(s"${hex(num)} ${pronounce(num)}")
    assert(pronounce(num) == pronunciation,
      s"Failed on ${hex(num)}: expected $pronunciation, got ${pronounce(num)}})")
  }

  check(0xA0, "atta")
  check(0xA000, "atta bitey")
  check(0xF5, "fleventy-five")
  check(0xB3, "bibbity-three")
  check(0xE4, "ebbity-four")
  check(0xBBBB, "bibbity-bee bitey bibbity-bee")
  check(0xA0C9, "atta bitey city-nine")
}
