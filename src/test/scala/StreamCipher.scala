import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object StreamCipher extends Properties("StreamCipher") {

  // see https://en.wikipedia.org/wiki/Linear_congruential_generator
  def lcg(m: Int, a: Int, c: Int)(seed: Int) = (a * seed + c) % m

  def rngStream(seed: Int): Stream[Byte] = {
    lazy val strm: Stream[Int] = seed #:: strm.scanLeft(seed) {
      case (_, next) => lcg(1 << 31, 1103515245, 12345)(next)
    }
    strm.tail.map(_.toByte)
  }

  def xor(tuple: (Byte, Byte)): Byte = (tuple._1 ^ tuple._2).toByte

  def enc(bytes: Stream[Byte], key: Int): Stream[Byte] = bytes.zip(rngStream(key)).map(xor)

  def encrypt(msg: String, key: Int): Array[Byte] = enc(msg.getBytes("utf-8").toStream, key).toArray

  def decrypt(crypto: Array[Byte], key: Int): String = new String(enc(crypto.toStream, key).toArray, "utf-8")

  property("round-trip") = forAll { (msg: String, key: Int) =>
    val ciphertext = encrypt(msg, key)
    val plaintext = decrypt(ciphertext, key)
    msg == plaintext
  }
}
