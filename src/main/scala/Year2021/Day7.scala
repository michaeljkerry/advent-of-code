package Year2021

import scala.io.Source.fromResource

object Day7 extends App {
  val input = fromResource("Year2021/Day7.txt").mkString.split(",").toList.map(_.toInt)

  val positionsToTry = 1 to 1000

  def calculateFuel(currentPosition: Int, targetPosition: Int): Int = Math.abs(currentPosition - targetPosition)

  def part2CalculateFuel(currentPosition: Int, targetPosition: Int): Int = {
    val movement = 1 to Math.abs(currentPosition - targetPosition)
    movement.sum
  }

  val positionFuelTuples = for {
    position <- positionsToTry.toList
    crabPosition <- input
    fuel = calculateFuel(crabPosition, position)
  } yield (position, fuel)

  val fuelByPosition = positionFuelTuples.groupBy(_._1)
  val totalFuelByPosition = fuelByPosition.map{case (k, v) => (k, v.map(_._2).sum)}
  val lowestPossibleFuel = totalFuelByPosition.minBy(_._2)._2
  println(lowestPossibleFuel)

  val part2PositionFuelTuples = for {
    position <- positionsToTry.toList
    crabPosition <- input
    fuel = part2CalculateFuel(crabPosition, position)
  } yield (position, fuel)

  val fuelByPositionPart2 = part2PositionFuelTuples.groupBy(_._1)
  val totalFuelByPositionPart2 = fuelByPositionPart2.map{case (k, v) => (k, v.map(_._2).sum)}
  val lowestPossibleFuelPart2 = totalFuelByPositionPart2.minBy(_._2)._2
  println(lowestPossibleFuelPart2)

}
