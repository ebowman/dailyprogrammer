import PileOfPaperFast.Rect
import org.scalatest.{FunSpec, Matchers}

class RectOpsSpec extends FunSpec with Matchers {

  describe("clipping") {
    it("should clip a known case correctly") {
      Rect(0, 4, 3, 3, 2).clip(Rect(0, 2, 4, 3, 4)) shouldEqual Rect(0, 4, 4, 1, 1)
    }
  }

  describe("rect intersection") {
    it("should handle non-intersection") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, -1, 0, 1, 1)) shouldEqual Set(Rect(0, 0, 0, 10, 10))
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, -1, 1, 1)) shouldEqual Set(Rect(0, 0, 0, 10, 10))
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 10, 0, 1, 1)) shouldEqual Set(Rect(0, 0, 0, 10, 10))
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 10, 1, 1)) shouldEqual Set(Rect(0, 0, 0, 10, 10))
    }
//    it("should handle the bottom only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 0, 10, 5)) shouldEqual Set(Rect(0, 0, 5, 10, 5))
//    }
//    it("should handle the top only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 5, 15, 15)) shouldEqual Set(Rect(0, 0, 0, 10, 5))
//    }
//    it("should handle the left only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 0, 5, 10)) shouldEqual Set(Rect(0, 0, 0, 5, 10))
//    }
//    it("should handle the right only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 0, 5, 10)) shouldEqual Set(Rect(0, 5, 0, 5, 10))
//    }
//    it("should handle the left & top only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 5, 5, 5)) shouldEqual Set(Rect(0, 0, 0, 10, 5), Rect(0, 0, 5, 5, 5))
//    }
//    it("should handle the right & top only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 5, 5, 5)) shouldEqual Set(Rect(0, 0, 0, 10, 5), Rect(0, 5, 5, 5, 5))
//    }
//    it("should handle the left & bottom only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 0, 5, 5)) shouldEqual Set(Rect(0, 0, 0, 5, 5), Rect(0, 0, 5, 10, 5))
//    }
//    it("should handle the right & bottom only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 0, 5, 5)) shouldEqual Set(Rect(0, 5, 0, 5, 5), Rect(0, 0, 5, 10, 5))
//    }
//    it("should handle the left & right only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 0, 4, 10)) shouldEqual Set(Rect(0, 0, 0, 5, 10), Rect(0, 9, 0, 1, 10))
//    }
//    it("should handle the top & bottom only case") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 5, 10, 4)) shouldEqual Set(Rect(0, 0, 0, 10, 5), Rect(0, 0, 9, 10, 1))
//    }
//    it("should handle the left, right & bottom") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 0, 4, 9)) shouldEqual
//        Set(Rect(0, 0, 0, 5, 9), Rect(0, 9, 0, 1, 9), Rect(0, 0, 9, 10, 1))
//    }
//    it("should handle the top, right & bottom") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 0, 1, 9, 8)) shouldEqual
//        Set(Rect(0, 0, 0, 10, 1), Rect(0, 9, 1, 1, 8), Rect(0, 0, 9, 10, 1))
//    }
//    it("should handle the top, left & bottom") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 1, 5, 8)) shouldEqual
//        Set(Rect(0, 0, 0, 10, 1), Rect(0, 0, 1, 5, 8), Rect(0, 0, 9, 10, 1))
//    }
//    it("should handle the top, left & right") {
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 5, 1, 4, 9)) shouldEqual
//        Set(Rect(0, 0, 0, 10, 1), Rect(0, 0, 1, 5, 9), Rect(0, 9, 1, 1, 9))
//    }
//    it("should handle top, left, right & bottom") {
//      Rect(0, 0, 0, 3, 3).cover(Rect(1, 1, 1, 1, 1)) shouldEqual
//        Set(Rect(0, 0, 0, 3, 1), Rect(0, 0, 1, 1, 1), Rect(0, 2, 1, 1, 1), Rect(0, 0, 2, 3, 1))
//      Rect(0, 0, 0, 10, 10).cover(Rect(1, 4, 3, 3, 2)) shouldEqual
//        Set(Rect(0, 0, 0, 10, 3), Rect(0, 0, 3, 4, 2), Rect(0, 7, 3, 3, 2), Rect(0, 0, 5, 10, 5))
//
//    }
//    it("should handle various known cases") {
//      Rect(0, 0, 3, 4, 2).cover(Rect(2, 2, 4, 3, 4)) shouldEqual Set(Rect(0, 0, 3, 4, 1), Rect(0, 0, 4, 2, 1))
//      Rect(1, 4, 3, 3, 2).cover(Rect(2, 2, 4, 3, 4)) shouldEqual Set(Rect(1, 4, 3, 3, 1), Rect(1, 5, 4, 2, 1))
//      Rect(0, 0, 0, 20, 5).cover(Rect(2, 0, 0, 7, 7)) shouldEqual Set(Rect(0, 7, 0, 13, 5))
//      Rect(0,0,45,25,54).cover(Rect(3,9,90,55,2)) shouldEqual Set(Rect(0,0,45,25,54))
//    }
    it("should find the missing point bug") {
      // (37520,37709) Rect(0,0,0,100000,100000) Rect(9,37520,37709,53884,43089) Set(Rect(0,0,0,100000,37709), Rect(0,0,37709,37520,43089), Rect(0,91404,37709,8596,43089), Rect(0,0,80798,100000,19202))
//      Rect(0, 0, 0, 100000, 100000).cover(Rect(9, 37520, 37709, 53884, 43089)) shouldEqual
//        Set(Rect(0,0,0,100000, 37709), Rect(0, 0, 37709, 37520, 43089),
//        Rect(0, 37520+53884, 37709, 100000 - 37520 - 53884, 43089), Rect(0, 0, 37709+43089, 100000, 100000 - 37709 - 43089))

      //(37520,80797)
      //Rect(9,37520,69871,53884,10927).cover(Rect(1,87567,76459,6275,7346) shouldEqual Set(
      Rect(9,37520,69871,53884,10927).cover(Rect(1,87567,76459,6275,7346)) shouldEqual ()
    }
  }
}
