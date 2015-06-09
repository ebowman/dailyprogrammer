object SpaceCodeBreaking extends App {

  val decryptors = Seq[(String, String => String)](
    "Omicron V" -> (_.map(c => (c ^ 0x10).toChar)),
    "Hoth" -> (_.map(c => (c - 10).toChar)),
    "Ryza IV" -> (_.map(c => (c + 1).toChar)),
    "Htrae" -> (_.reverse))

  def best(encrypted: String): Seq[String] = {
    def score(str: String): Int = str.count((c: Char) => c.isLetter || c.isSpaceChar)
    val best = (for {
      (name, decrypt) <- decryptors
      decrypted = decrypt(encrypted)
    } yield (name, decrypted, score(decrypted))).maxBy(_._3)
    Seq(best._1, best._2)
  }

  val encodedCyperText = Seq(
    " 71 117  48 115 127 125 117  48 121 126  48  96 117 113 115 117 ",
    " 97 111  42 109 121 119 111  42 115 120  42 122 111 107 109 111 ",
    " 86 100  31  98 110 108 100  31 104 109  31 111 100  96  98 100 ",
    " 101  99  97 101 112  32 110 105  32 101 109 111  99  32 101  87 ",
    " 84 113 121 124 105  48  64  98 127 119  98 113 125 125 117  98  48 121  99  48  99  96 105 121 126 119  48 127 126  48 101  99 ",
    " 78 107 115 118 131  42  90 124 121 113 124 107 119 119 111 124  42 115 125  42 125 122 131 115 120 113  42 121 120  42 127 125 ",
    " 67  96 104 107 120  31  79 113 110 102 113  96 108 108 100 113  31 104 114  31 114 111 120 104 109 102  31 110 109  31 116 114 ",
    " 115 117  32 110 111  32 103 110 105 121 112 115  32 115 105  32 114 101 109 109  97 114 103 111 114  80  32 121 108 105  97  68 ",
    " 86 121  98 117  48 100 120 117  48  93 121  99  99 124 117  99 ",
    " 80 115 124 111  42 126 114 111  42  87 115 125 125 118 111 125 ",
    " 69 104 113 100  31 115 103 100  31  76 104 114 114 107 100 114 ",
    " 115 101 108 115 115 105  77  32 101 104 116  32 101 114 105  70 "
  )

  val cypherText = encodedCyperText.map(line => line.trim.split("\\s+").map(_.toInt.toChar).mkString)

  cypherText.map(best).map(_.mkString(": ")).foreach(println)

}
