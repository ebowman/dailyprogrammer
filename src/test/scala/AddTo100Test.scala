import org.scalatest.{FunSpec, Matchers}

class AddTo100Test extends FunSpec with Matchers with Solver {

  describe("generate") {
    it("should generate known sequences") {
      generate(1 to 3).toSet shouldBe Set(
      "1.2.3", "1.2 + 3", "1.2 - 3",
      "1 + 2.3", "1 + 2 + 3", "1 + 2 - 3",
      "1 - 2.3", "1 - 2 + 3", "1 - 2 - 3"
      )
    }
    it("should solve the 100 problem") {
      solve(1 to 9).toSet shouldBe Set(
      "1 + 2 + 3 - 4 + 5 + 6 + 78 + 9",
      "1 + 2 + 34 - 5 + 67 - 8 + 9",
      "1 + 23 - 4 + 5 + 6 + 78 - 9",
      "1 + 23 - 4 + 56 + 7 + 8 + 9",
      "12 + 3 + 4 + 5 - 6 - 7 + 89",
      "12 + 3 - 4 + 5 + 67 + 8 + 9",
      "12 - 3 - 4 + 5 - 6 + 7 + 89",
      "123 + 4 - 5 + 67 - 89",
      "123 + 45 - 67 + 8 - 9",
      "123 - 4 - 5 - 6 - 7 + 8 - 9",
      "123 - 45 - 67 + 89"
      )
    }
  }
}

class Solver2Test extends FunSpec with Solver2 with Matchers {
  describe("solver2") {
    it("should work") {
      generate(0, 1 to 3) shouldBe Some("1.2.3")
      generate(1, 1 to 3) shouldBe Some("1.2+3")
      generate(2, 1 to 3) shouldBe Some("1.2-3")
      generate(3, 1 to 3) shouldBe Some("1+2.3")
      generate(4, 1 to 3) shouldBe Some("1+2+3")
      generate(5, 1 to 3) shouldBe Some("1+2-3")
      generate(6, 1 to 3) shouldBe Some("1-2.3")
      generate(7, 1 to 3) shouldBe Some("1-2+3")
      generate(8, 1 to 3) shouldBe Some("1-2-3")
      generate(9, 1 to 3) shouldBe None

      generate(0, 1 to 4) shouldBe Some("1.2.3.4")
      generate(1, 1 to 4) shouldBe Some("1.2.3+4")
      generate(2, 1 to 4) shouldBe Some("1.2.3-4")
      generate(3, 1 to 4) shouldBe Some("1.2+3.4")
      generate(4, 1 to 4) shouldBe Some("1.2+3+4")
      generate(5, 1 to 4) shouldBe Some("1.2+3-4")
      generate(6, 1 to 4) shouldBe Some("1.2-3.4")
      generate(7, 1 to 4) shouldBe Some("1.2-3+4")
      generate(8, 1 to 4) shouldBe Some("1.2-3-4")
      generate(9, 1 to 4) shouldBe Some("1+2.3.4")
      generate(10, 1 to 4) shouldBe Some("1+2.3+4")
      generate(11, 1 to 4) shouldBe Some("1+2.3-4")
      generate(12, 1 to 4) shouldBe Some("1+2+3.4")
      generate(13, 1 to 4) shouldBe Some("1+2+3+4")
      generate(14, 1 to 4) shouldBe Some("1+2+3-4")
      generate(15, 1 to 4) shouldBe Some("1+2-3.4")
      generate(16, 1 to 4) shouldBe Some("1+2-3+4")
      generate(17, 1 to 4) shouldBe Some("1+2-3-4")
      generate(18, 1 to 4) shouldBe Some("1-2.3.4")
      generate(19, 1 to 4) shouldBe Some("1-2.3+4")
      generate(20, 1 to 4) shouldBe Some("1-2.3-4")
      generate(21, 1 to 4) shouldBe Some("1-2+3.4")
      generate(22, 1 to 4) shouldBe Some("1-2+3+4")
      generate(23, 1 to 4) shouldBe Some("1-2+3-4")
      generate(24, 1 to 4) shouldBe Some("1-2-3.4")
      generate(25, 1 to 4) shouldBe Some("1-2-3+4")
      generate(26, 1 to 4) shouldBe Some("1-2-3-4")

      generate(27, 1 to 4) shouldBe None
    }
  }
}
