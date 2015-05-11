/**
 * Solution to http://www.reddit.com/r/dailyprogrammer/comments/358pfk/20150508_challenge_213_hard_stepstring_discrepancy/
 */
object StepstringDiscrepancy extends App {

  def discrepStep(str: Array[Int], x: Int, y: Int, n: Int): Int = {
    var sum = 0
    var i = x
    while (i <= y) {
      sum += str(i)
      i += n
    }
    math.abs(sum)
  }

  def discrepancyStream(str: String): Stream[Int] = {
    val reduced = str.map {
      case 'a' => 1
      case 'b' => -1
    }.toArray

    for {
      x <- Stream.range(0, str.length - 1)
      y <- Stream.range(x + 1, str.length)
      n <- Stream.range(1, str.length)
    } yield discrepStep(reduced, x, y, n)
  }

  assert(discrepancyStream("bbaaabababbaabbaaaabbbababbaabbbaabbaaaaabbababaaaabaabbbaaa").max == 9)
  assert(discrepancyStream("bbaaaababbbaababbbbabbabababababaaababbbbbbaabbaababaaaabaaa").max == 12)
  assert(discrepancyStream("aaaababbabbaabbaabbbbbbabbbaaabbaabaabaabbbaabababbabbbbaabb").max == 11)
  assert(discrepancyStream("abbabbbbbababaabaaababbbbaababbabbbabbbbaabbabbaaabbaabbbbbb").max == 15)

  io.Source.fromFile("src/main/resources/steps.txt").getLines().map {
    line =>
      println(line)
      discrepancyStream(line).max
  }.foreach(println)
}

object Try2 extends App {
  def maxSubs(in: String): Int = {
    val v = in.collect {
      case 'a' => 1
      case 'b' => -1
    }.toArray
    var maxDiscrepancy = 0
    var step = 1
    while (step < v.length) {
      var start = 0
      var done = false
      while (!done && start < step) {
        if ((v.length - start) / step < maxDiscrepancy) done = true
        else {
          var (sumAs, sumBs, maxAs, maxBs) = (0, 0, 0, 0)
          var i = start
          while (i < v.length) {
            sumAs = math.max(0, sumAs + v(i))
            maxAs = math.max(maxAs, sumAs)
            sumBs = math.max(0, sumBs - v(i))
            maxBs = math.max(maxBs, sumBs)
            i += step
          }
          maxDiscrepancy = math.max(maxDiscrepancy, math.max(maxAs, maxBs))
        }
        start += 1
      }
      step += 1
    }
    maxDiscrepancy
  }

  assert(maxSubs("bbaaabababbaabbaaaabbbababbaabbbaabbaaaaabbababaaaabaabbbaaa") == 9)
  assert(maxSubs("bbaaaababbbaababbbbabbabababababaaababbbbbbaabbaababaaaabaaa") == 12)
  assert(maxSubs("aaaababbabbaabbaabbbbbbabbbaaabbaabaabaabbbaabababbabbbbaabb") == 11)
  assert(maxSubs("abbabbbbbababaabaaababbbbaababbabbbabbbbaabbabbaaabbaabbbbbb") == 15)
  io.Source.fromURL("http://pastebin.com/raw.php?i=Xt3BV8nK").getLines().map(maxSubs).foreach(println)
}
