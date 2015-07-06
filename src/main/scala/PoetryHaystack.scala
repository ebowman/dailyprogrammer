
object PoetryHaystack extends App {

  val haystackUrl = "https://gist.githubusercontent.com/anonymous/c8fb349e9ae4fcb40cb5/raw/05a1ef03626057e1b57b5bbdddc4c2373ce4b465/challenge.txt"
  val wordsUrl = "http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt"

  val dict = io.Source.fromURL(wordsUrl).getLines().toSet

  val lines = io.Source.fromURL(haystackUrl).getLines().toSeq

  def score(line: String): Int = line.split("\\s+").map(w => if (dict.contains(w)) 1 else 0).sum

  val start = System.currentTimeMillis()
  val scored = lines.map(l => (l, -score(l)))
  scored.sortBy(_._2).take(3).foreach(println)
  val done = System.currentTimeMillis()
  println(done - start)

}
