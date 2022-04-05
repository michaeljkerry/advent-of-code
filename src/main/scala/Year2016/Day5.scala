package Year2016

import java.security.MessageDigest
import scala.annotation.tailrec

object Day5 extends App {

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  def convertToMd5Hex(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest: Array[Byte] = md.digest(input.getBytes)
    convertBytesToHex(digest)
  }

  @tailrec
  def runChessPart1(input: String, password: String, digit: Int): String = {
    val md5Hex = convertToMd5Hex(input + digit.toString)
    val newPassword = if (md5Hex.startsWith("00000")) password + md5Hex.charAt(5) else password
    if (newPassword.length == 8) newPassword
    else runChessPart1(input, newPassword, digit + 1)
  }

  val part1Answer = runChessPart1("reyedfim", "", 0)
  println(part1Answer)

  @tailrec
  def runChessPart2(input: String, passwordMap: Map[Int, Char], digit: Int): String = {
    val md5Hex = convertToMd5Hex(input + digit.toString)
    val newPassword = if (md5Hex.startsWith("00000") && md5Hex.charAt(5).isDigit && md5Hex.charAt(5).toString.toInt < 8 && !passwordMap.contains(md5Hex.charAt(5).toString.toInt))
      passwordMap ++ Map(md5Hex.charAt(5).toString.toInt -> md5Hex.charAt(6))
    else passwordMap
    if (newPassword.keys.toList.length == 8) newPassword.toList.sortBy(_._1).map(_._2).mkString
    else runChessPart2(input, newPassword, digit + 1)
  }

  val part2Answer = runChessPart2("reyedfim", Map.empty[Int, Char], 0)
  println(part2Answer)

}
