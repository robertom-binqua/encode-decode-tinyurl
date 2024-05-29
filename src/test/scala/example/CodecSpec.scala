package org.binqua.examples.tinyurl

class CodecSpec extends munit.FunSuite {

  test("BigInt(62).pow(6) is baaaaaa") {
    assertEquals(Codec.from10To62EncodingTailRec(BigInt(62).pow(6)), "baaaaaa")
  }

  test("2 * BigInt(62).pow(6) is 1000000") {
    assertEquals(Codec.from10To62EncodingTailRec(BigInt(2) * BigInt(62).pow(6)), "caaaaaa")
  }

  test("BigInt(62).pow(1) is aaaaaba") {
    assertEquals(Codec.from10To62EncodingTailRec(BigInt(62).pow(1)), "aaaaaba")
  }

  test("BigInt(61) is aaaaaa9") {
    assertEquals(Codec.from10To62EncodingTailRec(BigInt(61)), "aaaaaa9")
  }

  test("fromShortURLToDecimalKey of aaaaaa9 is 61") {
    assertEquals(Codec.from62To10EncodingTailRec("aaaaaa9"), BigInt(61))
  }

  test("fromShortURLToDecimalKeyRec of from10To62EncodingTailRec returns original value") {
    val result = BigInt("101000100000")
    assertEquals(Codec.from10To62EncodingTailRec(result), "bWpqh08")
    assertEquals(Codec.from62To10EncodingTailRec(Codec.from10To62EncodingTailRec(result)), result)
  }

  test("firs encode and then decode creates the original value") {
    val codec = new Codec()
    assertEquals(codec.encode("https://leetcode.com/problems/design-tinyurl1"), "http://tinyurl.com/abfP3Qq")
    assertEquals(codec.encode("https://leetcode.com/problems/design-tinyurl2"), "http://tinyurl.com/abfP3Qr")
    assertEquals(codec.encode("https://leetcode.com/problems/design-tinyurl3"), "http://tinyurl.com/abfP3Qs")

    assertEquals(codec.decode("http://tinyurl.com/abfP3Qq"),"https://leetcode.com/problems/design-tinyurl1")
    assertEquals(codec.decode("http://tinyurl.com/abfP3Qr"),"https://leetcode.com/problems/design-tinyurl2")
    assertEquals(codec.decode("http://tinyurl.com/abfP3Qs"),"https://leetcode.com/problems/design-tinyurl3")
  }

}
