import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}

class BruteForceTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  val numGen = Gen.choose(0, 9999)
  val numSeqGen = Gen.nonEmptyListOf(numGen)

  describe("sorting") {
    it("sorting should match the brute force algo") {
      forAll(numSeqGen) {
        nums => Algorithms.sorting(nums.take(5)) shouldEqual Algorithms.bruteForce(nums.take(5))
      }
    }
//    it("broken should match the brute force algo") {
//      forAll(numSeqGen) {
//        nums => Algorithms.broken(nums) shouldEqual Algorithms.sorting(nums)
//      }
//    }
  }
}
