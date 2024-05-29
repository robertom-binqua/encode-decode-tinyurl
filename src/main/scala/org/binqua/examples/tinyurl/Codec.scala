package org.binqua.examples.tinyurl

import scala.annotation.tailrec
import scala.collection.mutable

object Codec {

  val values: Array[Char] = (('a' to 'z').toList ::: ('A' to 'Z').toList ::: ('0' to '9').toList).toArray

  def from62To10EncodingTailRec(shortURL: String): BigInt = {
    @tailrec
    def doIt(acc: BigInt, string: List[Char], index: Int): BigInt = {
      string match {
        case ::(head, next) => doIt(acc + BigInt(values.indexOf(head)) * BigInt(62).pow(index), next, index + 1)
        case Nil            => acc
      }
    }
    if (shortURL.length > 7) throw new IllegalArgumentException(s"$shortURL too long")
    doIt(BigInt(0), shortURL.reverse.toList, 0)
  }

  def from10To62EncodingTailRec(decimalKey: BigInt): String = {
    @tailrec
    def doIt(decimalKey: BigInt, result: String, index: Int): String = {
      if (index >= 7)
        result
      else {
        val digits: BigInt = decimalKey % 62
        doIt(decimalKey / 62, s"${values(digits.toInt).toString}$result", index + 1)
      }
    }

    doIt(decimalKey = decimalKey, result = "", index = 0)
  }

}

class Codec {

  private var currentKey: BigInt = 1000000000
  private val fromLongUrlToDecimalKeyMap: mutable.Map[String, BigInt] = new mutable.HashMap()
  private val fromDecimalKeyToLongUrlMap: mutable.Map[BigInt, String] = new mutable.HashMap()

  private val shortUrlPrefix = "http://tinyurl.com/"

  def encode(longURL: String): String = {
    val decimalKey = (fromLongUrlToDecimalKeyMap.get(longURL) match {
      case Some(decimalKey) => decimalKey
      case None =>
        val key = currentKey
        fromLongUrlToDecimalKeyMap.put(longURL, key)
        currentKey += 1
        key
    })
    fromDecimalKeyToLongUrlMap.put(decimalKey, longURL)
    val shortSuffix = Codec.from10To62EncodingTailRec(decimalKey)
    s"$shortUrlPrefix$shortSuffix"
  }

  def decode(shortURL: String): String = {
    val decimalKey = Codec.from62To10EncodingTailRec(shortURL.replace(shortUrlPrefix, ""))
    fromDecimalKeyToLongUrlMap.getOrElse(
      key = decimalKey,
      default = throw new IllegalStateException("Not found")
    )
  }

}
