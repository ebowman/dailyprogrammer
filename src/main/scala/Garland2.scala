object Garland2 extends App {

  def garland(s: String): Int = {
    @scala.annotation.tailrec
    def recur(t: String): Int = {
      if (t.isEmpty) 0
      else if (s.startsWith(t)) t.length
      else recur(t.tail)
    }
    recur(s.tail)
  }

  assert(garland("onion") == 2)
  assert(garland("foo") == 0)
  assert(garland("alfalfa") == 4)

  def f(x: ((String, String)) => String): String= { x("3", "4") }

  f((x: (String,String)) => x._1 + x._2)
  f{ case (a, b) => a + b }
}
