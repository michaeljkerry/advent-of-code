package Year2015

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day1 extends App {
  val input = fromResource("Year2015/Day1.txt")
    .getLines()
    .toList

  case class Floors(down: Int, up: Int, floorsProcessed: Int, currentFloor: Int)

  def processFloor(nextFloor: Char, floorState: Floors): Floors = {
    val newFloorState = if (nextFloor == ')') {
      floorState.copy(down = floorState.down + 1, floorsProcessed = floorState.floorsProcessed + 1, currentFloor = floorState.currentFloor - 1)
    } else {
      floorState.copy(up = floorState.up + 1, floorsProcessed = floorState.floorsProcessed + 1, currentFloor = floorState.currentFloor + 1)
    }
    newFloorState
  }
  @tailrec
  def processDelivery(floorsToProcess: String, floors: Floors): Floors = {
    floorsToProcess.headOption match {
      case None => floors
      case Some(f) => {
        val processed = processFloor(f, floors)
        processDelivery(floorsToProcess.tail, processed)
      }
    }
  }

  @tailrec
  def processDeliveryUntilBasement(floorsToProcess: String, floors: Floors): Floors = {
    floorsToProcess.headOption match {
      case None => floors
      case Some(f) => {
        val processed = processFloor(f, floors)
        if (processed.currentFloor == -1) processed
        else processDeliveryUntilBasement(floorsToProcess.tail, processed)
      }
    }
  }

  println(input)

  val resultPart1 = processDelivery(input.head, Floors(0, 0, 0, 0))
  println(s"resultPart1: $resultPart1")

  val resultPart2 = processDeliveryUntilBasement(input.head, Floors(0, 0, 0, 0))
  println(s"resultPart2: $resultPart2")


}
