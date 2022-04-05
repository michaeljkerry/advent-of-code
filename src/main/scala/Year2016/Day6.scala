package Year2016

import scala.io.Source.fromResource

object Day6 extends App {
  val input = fromResource("Year2016/Day6.txt")
    .getLines()
    .toList
    .map(_.toList)

  val part1Answer = input
    .transpose
    .map(_.groupBy(identity).view.mapValues(_.length).toList.maxBy(_._2))
    .map(_._1)
    .mkString

  println(part1Answer)

  val part2Answer = input
    .transpose
    .map(_.groupBy(identity).view.mapValues(_.length).toList.minBy(_._2))
    .map(_._1)
    .mkString

  println(part2Answer)
}
