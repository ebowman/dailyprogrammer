import java.util.Comparator
import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.mutable

object EelOfFortune extends App {

  def problem(problem: String)(word: String): Boolean = {
    word.collect {
      case l if problem.contains(l) => l
    }.mkString == problem
  }

  def snond = problem("snond") _

  assert(snond("synchronized"))
  assert(snond("misfunctioned"))
  assert(!snond("mispronounced"))
  assert(!snond("shotgunned"))
  assert(snond("snond"))

  val words = io.Source.fromURL(
    "https://dotnetperls-controls.googlecode.com/files/enable1.txt").getLines().toStream
  assert(words.count(snond) == 6)

  def rrizi = problem("rrizi") _

  println(s"rrizi count = ${words.par.count(rrizi)}")

  def combs(word: String, size: Int = 5): Seq[String] = {
    if (size == 0 || word.length < size) Nil
    else if (word.length == size) Seq(word)
    else (word.take(size) +:
      (combs(word.tail, size - 1).map(p => word.head + p) ++
        combs(word.tail, size))).distinct
  }

  import scala.collection.JavaConverters._

  val topScores: mutable.Set[(String, Int)] =
    new ConcurrentSkipListSet[(String, Int)](new Comparator[(String, Int)] {
    override def compare(x: (String, Int), y: (String, Int)): Int =
      if (x._2 < y._2) -1
      else if (x._2 == y._2) if (x._2 < 500) 0 else x._1.compare(y._1)
      else 1
  }).asScala

  val start = System.currentTimeMillis()
  words.par.filter(_.length > 5).foreach { case word =>
    topScores.add(word, combs(word).count(p => problem(p)(word)))
  }
  println(s"${System.currentTimeMillis() - start} ms")
  println(topScores.takeRight(10).toSeq.sortBy(_._2).reverse.mkString("\n"))
}

