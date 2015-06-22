package texscii

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object TeXSCII extends RegexParsers with PackratParsers with App {

  sealed trait TextBlock {
    def text: Seq[String]

    def width = text.map(_.length).max

    def height = text.size

    def print: String = text.mkString("\n")

    override def toString = print

    def vcenter(height: Int): TextBlock = {
      require(height >= this.height)
      if (height == this.height) this
      else {
        val buffer = height - this.height
        val ourText = text
        val ourWidth = width
        if ((buffer % 2) == 0) {
          new TextBlock {
            val text = (1 to buffer / 2).map(_ => " " * ourWidth) ++ ourText ++ (1 to buffer / 2).map(_ => " " * ourWidth)
          }
        }
        else {
          new TextBlock {
            val text = (1 to buffer / 2).map(_ => " " * ourWidth) ++ ourText ++ (1 to buffer / 2).map(_ => " " * ourWidth)
          }
        }
      }
    }
  }

  def center(width: Int)(str: String): String = {
    val buffer = width - str.length
    if ((buffer % 2) == 0) {
      (" " * (buffer / 2)) + str + (" " * (buffer / 2))
    } else {
      (" " * (buffer / 2)) + str + (" " * (1 + buffer / 2))
    }
  }

  def append(blocks: Seq[TextBlock]): TextBlock = {
    require(blocks.tail.forall(_.height == blocks.head.height))
    if (blocks.size == 1) blocks.head
    else new TextBlock {
      val text = for {
        y <- 0 until blocks.head.height
      } yield {
          blocks.map(block => block.text(y)).mkString
        }
    }
  }

  case class Token(value: String) extends TextBlock {
    def text = Seq(value)
  }

  case class Number(value: Int) extends TextBlock {
    def text = Seq(value.toString)
  }

  case class Frac(top: TextBlock, bottom: TextBlock) extends TextBlock {
    lazy val text: Seq[String] = {
      val width = math.max(top.width, bottom.width)
      top.text.map(center(width)) ++ Seq("-" * width) ++ bottom.text.map(center(width))
    }
  }

  case class Root(power: Int, arg: TextBlock) extends TextBlock {
    lazy val text: Seq[String] = {
      val inner = arg.text
      val innerHeight = arg.height
      val top = if (power == 2) " " * innerHeight else " " * (innerHeight - power.toString.length) + power
      val topBorder = top + "_" * arg.width
      val radical = for (i <- 0 until innerHeight) yield {
        if (i == innerHeight - 1) "√" + " " * i
        else " " * (innerHeight - i - 1) + "/" + " " * i
      }
      topBorder +: radical.zip(inner).map(ab => ab._1 + ab._2)
    }
  }

  case object Pi extends TextBlock {
    val text = Seq("π")
  }

  case class Super(context: TextBlock, arg: TextBlock) extends TextBlock {
    lazy val text: Seq[String] = {
      val topBlock = arg.text.map(suffix => " " * context.width + suffix)
      val bottomBlock = context.text.map(prefix => prefix + " " * arg.width)
      topBlock ++ bottomBlock
    }
  }

  case class Sub(context: TextBlock, arg: TextBlock) extends TextBlock {
    lazy val text: Seq[String] = {
      val topBlock = context.text.map(prefix => prefix + " " * arg.width)
      val bottomBlock = arg.text.map(suffix => " " * context.width + suffix)
      topBlock ++ bottomBlock
    }
  }

  case class SuperSub(context: TextBlock, sup: TextBlock, sub: TextBlock) extends TextBlock {
    lazy val text: Seq[String] = {
      val supSubWidth = math.max(sup.width, sub.width)
      val topBlock = sup.text.map(suffix => " " * context.width + center(supSubWidth)(suffix))
      val middleBlock = context.text.map(prefix => prefix + " " * supSubWidth)
      val bottomBlock = sub.text.map(suffix => " " * context.width + center(supSubWidth)(suffix))
      topBlock ++ middleBlock ++ bottomBlock
    }
  }

  def print(t: TextBlock): Unit = println(t.text.mkString("\n"))

  //  println(Sqrt(3, Frac(Number(100), Number(20000))).print)
  //  println(Super(Sqrt(2, Number(2)), Number(100)).print)
  //  println(Super(Number(5), Number(3)).print)
  //  println(Super(Number(5), Number(3)).print)
  //  println(Sub(Token("N"), Number(12)).print)
  //  println(SuperSub(Token("N"), Number(12000), Number(2)).print)

  lazy val intArg: PackratParser[Int] = log("{" ~> """[\d+]+""".r <~ "}" ^^ { case num => num.toInt })("intArg")

  lazy val arg: PackratParser[TextBlock] = log(("{" ~> expr) <~ "}" ^^ { case any => any })("arg")

  lazy val token: PackratParser[TextBlock] = log("""[^\\^{}_]+""".r ^^ { case str => println(s"Found token $str"); Token(str) })("token")

  lazy val pi: PackratParser[TextBlock] = log("""\pi""" ^^ { case _ => Pi })("pi")

  lazy val frac: PackratParser[TextBlock] = log("""\frac""" ~> arg ~ arg ^^ { case top ~ bottom => Frac(top, bottom) })("frac")

  lazy val sqrt: PackratParser[TextBlock] = """\sqrt""" ~> arg ^^ { case arg => Root(2, arg) }

  lazy val root: PackratParser[TextBlock] = """\root""" ~> intArg ~ arg ^^ { case power ~ arg => Root(power, arg) }

  lazy val sub: Parser[TextBlock] = (expr <~ "_") ~ arg ^^ { case context ~ sub => Sub(context, sub) }

  lazy val sup: PackratParser[TextBlock] = log((expr <~ "^") ~ arg ^^ { case context ~ sub => Super(context, sub) })("sup")

  lazy val supSub: Parser[TextBlock] = (expr <~ "_") ~ arg ~ ("^" ~> arg) ^^ { case context ~ sub ~ sup => SuperSub(context, sup, sub) }

  lazy val any: PackratParser[TextBlock] = token | pi | frac | sqrt | root | sup | supSub | sub | sup

  lazy val expr: PackratParser[TextBlock] = rep1(any) ^^ { case bits => append(bits.map(_.vcenter(bits.map(_.height).max))) }

  def parseAll[T](p: Parser[T], input: String) = phrase(p)(new PackratReader(new CharSequenceReader(input)))

  println(parseAll(expr, "77+22^{22}"))
  //println(parseAll(any, "\\pi^{2}"))
  //println(parseAll(any, "\\pi^{2}"))
  //  println(parseAll(arg, "{x^{2}}"))
  //  println(parseAll(any, "\\sqrt{2}"))
  //println(parseAll(any, "\\root{2}{x^2}"))
  //println(parseAll(sub, "foo_{e}"))
  //println(parseAll(any, "log _{e}"))

  //println(parseAll(expr, "log_{e}(e^{x})=x").get.print)
}

