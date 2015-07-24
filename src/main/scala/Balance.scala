object Balance extends App {

  def balance(s: String): Boolean = {
    var fail = false
    val f = s.foldLeft(0) {
      case (s, c) if s < 0 =>
        fail = true
        c
      case (s, c) => c match {
        case '(' => s + 1
        case ')' => s - 1
        case _ => s
      }
    }
    println(s"s = $s, f = $f, fail = $fail")
    if (fail) false
    else f == 0
  }

  assert(balance("((()))"))
  assert(!balance(")))((("))
  assert(balance("()()()()"))
  assert(!balance("()()())"))
  assert(!balance("()()()("))
}
