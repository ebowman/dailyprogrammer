package texscii

import scala.util.parsing.combinator.RegexParsers

object TeXSCII extends RegexParsers with App {

  sealed trait TextBlock {
    def text: Seq[String]

    def width = text.map(_.length).max

    def height = text.size

    def print: String = text.mkString("\n")

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
    new TextBlock {
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

  def intArg: Parser[Int] = "{" ~> """[\d+]+""".r <~ "}" ^^ { case num => num.toInt }

  def arg: Parser[TextBlock] = ("{" ~> expr) <~ "}" ^^ { case any => any }

  def token: Parser[TextBlock] = """[^\\^{}_]+""".r ^^ { case str => println(s"Found token $str"); Token(str) }

  def pi: Parser[TextBlock] = """\pi""" ^^ { case _ => Pi }

  def frac: Parser[TextBlock] = """\frac""" ~> arg ~ arg ^^ { case top ~ bottom => Frac(top, bottom) }

  def sqrt: Parser[TextBlock] = """\sqrt""" ~> arg ^^ { case arg => Root(2, arg) }

  def root: Parser[TextBlock] = """\root""" ~> intArg ~ arg ^^ { case power ~ arg => Root(power, arg) }

//  def sub: Parser[TextBlock] = (expr <~ "_") ~ arg ^^ { case context ~ sub => Sub(context, sub) }
//
//  def sup: Parser[TextBlock] = (expr <~ "^") ~ arg ^^ { case context ~ sub => Super(context, sub) }
//
//  def supSub: Parser[TextBlock] = (expr <~ "_") ~ arg ~ ("^" ~> arg) ^^ { case context ~ sub ~ sup => SuperSub(context, sup, sub) }

  def any: Parser[TextBlock] = token | pi | frac | sqrt | root //| supSub | sub | sup

  def expr: Parser[TextBlock] = rep1(any) ^^ { case bits => append(bits.map(_.vcenter(bits.map(_.height).max))) }

  println(parseAll(expr, "77"))
  //println(parseAll(any, "\\pi^{2}"))
  //println(parseAll(any, "\\pi^{2}"))
//  println(parseAll(arg, "{x^{2}}"))
//  println(parseAll(any, "\\sqrt{2}"))
  //println(parseAll(any, "\\root{2}{x^2}"))
  //println(parseAll(sub, "foo_{e}"))
  //println(parseAll(any, "log _{e}"))

  //println(parseAll(expr, "log_{e}(e^{x})=x").get.print)
}

