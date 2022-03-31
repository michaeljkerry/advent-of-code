package Year2016

import scala.io.Source.fromResource

object Day3 extends App {
  val input = fromResource("Year2016/Day3.txt")
    .getLines()
    .toList
    .map(_.trim)
    .map(_.split("\\s+").toList.map(_.toInt))

  println(input)

  def validateTriangle(sides: List[Int]): Boolean =
    (sides(0) + sides(1) > sides(2)) && (sides(0) + sides(2) > sides(1)) && (sides(1) + sides(2) > sides(0))

  val resultPart1 = input.count(validateTriangle)

  println(resultPart1)

  val resultPart2 = input
    .transpose
    .map(_.grouped(3).toList)
    .map(l => l.count(validateTriangle))
    .sum

  println(resultPart2)

}
