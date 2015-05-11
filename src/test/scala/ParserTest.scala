import org.scalatest.{Matchers, FunSpec}

class ParserTest extends FunSpec with Matchers with Parser {

  describe("parser") {
    it ("should parse a num") {
      parseAll(number, "7").get shouldBe 7
    }
    it ("should parse a comb") {
      parseAll(combiner, ". 7").get(4) shouldBe 47
    }
    it ("should combine a comb") {
      parseAll(combined, "4 . 7").get shouldBe 47
    }
    it ("parse subexpressions") {
      parseAll(subexpr, "77").get shouldBe 77
      parseAll(subexpr, "7.7").get shouldBe 77
      parseAll(subexpr, "7.7.7").get shouldBe 777
    }
    it ("should parse add") {
      parseAll(adder, "+40").get(10) shouldBe 50
    }
    it ("should parse sub") {
      parseAll(subtractor, "- 40").get(10) shouldBe -30
    }
    it ("should parse an expression") {
      parseAll(expr, "3+4").get shouldBe 7
      parseAll(expr, "3.5+4").get shouldBe 39
      parseAll(expr, "4+3.5+4").get shouldBe 43
    }
  }
}
