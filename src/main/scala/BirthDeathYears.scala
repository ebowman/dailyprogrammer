object BirthDeathYears extends App {

  case class Person(birthYear: Int, deathYear: Int, name: String)

  val records = io.Source.fromString(
    """
      |Joe 1935 1988
      |Bob 1955 1978
      |Dylan 1900 1999
      |Chelsey 1905 1944
      |Greg 1985 2000
    """.stripMargin.trim).getLines().map { case line =>
    val bits: Array[String] = line.split("\\s+")
    Person(bits(1).toInt, bits(2).toInt, bits(0))
  }.toSeq

  val maxYear = (1900 to 2000).foldLeft((1900, 0, Int.MinValue, 1899)) {
    case ((curYear, curAlive, maxAlive, maxAliveYear), person) =>
      records.foldLeft((curYear, curAlive, maxAlive, maxAliveYear)) {
        case ((curYear, curAlive, maxAlive, maxAliveYear), person) =>
          if (curYear == person.birthYear) {
            val newMax = math.max(curAlive + 1, maxAlive)
            val newMaxYear = if (curAlive + 1 > maxAlive) curYear else maxAliveYear
            (curYear + 1, curAlive + 1, newMax, newMaxYear)
          } else if (curYear == person.deathYear) {
            (curYear + 1, curAlive - 1, maxAlive, maxAliveYear)
          } else {
            (curYear + 1, curAlive, maxAlive, maxAliveYear)
          }
      }
  }._4

  println(maxYear)

}
