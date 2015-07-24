
object HeighwayDragonBigInt extends App {

   type Pt = (BigInt, BigInt)

   val Zero = BigInt(0)
   val One = BigInt(1)
   val MinusOne = BigInt(-1)

   def heighwayStream: Stream[Pt] = {

     def next(in: Stream[(Pt, Int)]): Stream[(Pt, Int)] = {
       def turn(n: Int) = (((n & (n * -1)) << 1) & n) != 0
       def nextTurn(x: Pt, y: Pt, n: Int) = if (turn(n)) turnRight(x, y) else turnLeft(x, y)
       val Stream(p0, p1) = in.takeRight(2).unzip._1
       val lastIdx = in.last._2
       val p2 = nextTurn(p0, p1, lastIdx)
       val p3 = nextTurn(p1, p2, lastIdx + 1)
       val p4 = nextTurn(p2, p3, lastIdx + 2)
       in #::: next(Stream(p2, p3, p4).zipWithIndex.map(vi => (vi._1, vi._2 + lastIdx + 1)))
     }

     def turnRight(p0: Pt, p1: Pt): Pt =
       (p1._1 - p0._1, p1._2 - p0._2) match {
         case (One, Zero) => (p1._1, p1._2 - 1)
         case (MinusOne, Zero) => (p1._1, p1._2 + 1)
         case (Zero, One) => (p1._1 + 1, p1._2)
         case (Zero, MinusOne) => (p1._1 - 1, p1._2)
       }

     def turnLeft(p0: Pt, p1: Pt): Pt =
       (p1._1 - p0._1, p1._2 - p0._2) match {
         case (One, Zero) => (p1._1, p1._2 + 1)
         case (MinusOne, Zero) => (p1._1, p1._2 - 1)
         case (Zero, One) => (p1._1 - 1, p1._2)
         case (Zero, MinusOne) => (p1._1 + 1, p1._2)
       }

     next(Stream((Zero, Zero), (One, Zero)).zipWithIndex).map(_._1)
   }

   def heighway(n: Int) = {
     require(math.round(math.pow(2, n)) < Int.MaxValue, s"${math.round(math.pow(2, n))} >= ${Int.MaxValue}")
     heighwayStream.take(math.round(math.pow(2, n)).toInt + 1)
   }

   val (x12, y12) = heighway(12).unzip

   assert(x12.sum == BigInt(-104896))
   assert(y12.sum == BigInt(52416))

   val start = System.currentTimeMillis()
   heighway(30).toIterator.foreach(identity)
   val end = System.currentTimeMillis()
   println(s"n = 30 took ${end - start} ms")
 }
