package Year2021

import scala.io.Source.fromResource

object Day2 extends App {
  val input = fromResource("Year2021/Day2.txt")
    .getLines()
    .toList
    .map(i => i.split(' ').toList)
    .map(l => (l(0), l(1).toInt))

  case class Position(horizontal: Int, depth: Int)
  case class PositionWithAim(horizontal: Int, depth: Int, aim: Int)

  val positionAfterSteps = input.foldLeft(Position(0, 0)){ (currentPosition, instruction) => {
    instruction._1 match {
      case "forward" => currentPosition.copy(horizontal = currentPosition.horizontal + instruction._2)
      case "down" => currentPosition.copy(depth = currentPosition.depth + instruction._2)
      case "up" => currentPosition.copy(depth = currentPosition.depth - instruction._2)
      case _ => currentPosition
    }
  }}

  val part1Answer = positionAfterSteps.horizontal * positionAfterSteps.depth

  println(part1Answer)

  val positionWithAimAfterSteps = input.foldLeft(PositionWithAim(0, 0, 0)){ (currentPosition, instruction) => {
    instruction._1 match {
      case "forward" => currentPosition.copy(horizontal = currentPosition.horizontal + instruction._2, depth = currentPosition.depth + (currentPosition.aim * instruction._2))
      case "down" => currentPosition.copy(aim = currentPosition.aim + instruction._2)
      case "up" => currentPosition.copy(aim = currentPosition.aim - instruction._2)
      case _ => currentPosition
    }
  }}

  val part2Answer = positionWithAimAfterSteps.horizontal * positionWithAimAfterSteps.depth

  println(part2Answer)
}
