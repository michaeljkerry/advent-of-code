package Year2016

import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day4 extends App {
  val rooms = fromResource("Year2016/Day4.txt")
    .getLines()
    .toList
    .map(parseInput)

  case class Room(name: String, sectorId: Int, checksum: String)

  def parseInput(line: String): Room = {
    val roomGroups: Regex = raw"^(.*?)(\d+)(\[.*\])".r
    line match {
      case roomGroups(n, s, c) => Room(n.dropRight(1), s.toInt, c.replaceAll("[\\[\\]]", ""))
    }
  }

  def mostFrequentChars(name: String): String =
    name
      .filterNot(_ == '-')
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toList
      .groupBy(_._2)
      .view
      .mapValues(v => v.map(_._1).sorted.mkString)
      .toList
      .sortBy(_._1)
      .reverse
      .map(_._2)
      .mkString
      .substring(0, 5)

  val part1Answer = rooms.filter(r => mostFrequentChars(r.name) == r.checksum).map(_.sectorId).sum

  println(part1Answer)

  def shiftCipher(name: String, id: Int): String = {
    val alphabetCount = id / 26 + 2
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val alphabets = (1 to alphabetCount).foldLeft(List.empty[String]){ (state, _) => state :+ alphabet}.mkString
    name.map(l => shiftChar(l, id, alphabets))
  }

  def shiftChar(char: Char, id: Int, alphabets: String): Char = {
    if (char == '-') ' '
    else {
      val startIndex = alphabets.indexOf(char)
      alphabets.charAt(startIndex + id)
    }
  }

  val part2Answer = rooms
    .filter(r => shiftCipher(r.name, r.sectorId).contains("northpole"))
    .map(_.sectorId)
    .head

  println(part2Answer)

  assert(parseInput("aaaaa-bbb-z-y-x-123[abxyz]") == Room("aaaaa-bbb-z-y-x", 123, "abxyz"))
  assert(mostFrequentChars("aaaaabbbzyx") == "abxyz")
  assert(mostFrequentChars("abcdefgh") == "abcde")
  assert(shiftCipher(name = "qzmt-zixmtkozy-ivhz", id = 343) == "very encrypted name")
}
