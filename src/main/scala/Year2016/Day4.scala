package Year2016

import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day4 extends App {
  val input = fromResource("Year2016/Day4Test.txt")
    .getLines()
    .toList
    .map(parseInput)

  case class Room(name: String, sectorId: Int, checksum: String)

  def parseInput(line: String): Room = {
    val roomGroups: Regex = raw"^(.*?)(\d+)(\[.*\])".r
    line match {
      case roomGroups(n, s, c) => Room(n.replace("-", ""), s.toInt, c.replaceAll("[\\[\\]]", ""))
    }
  }

  def mostFrequentChars(name: String): String =
    name
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(_._2)
      .reverse
      .map(_._1)
      .mkString

  println(input)

  println("aaaaabbbzyx".groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2).groupBy(_._2).filter(_._2.size > 1))
}
