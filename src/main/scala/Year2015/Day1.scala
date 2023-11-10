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
    println(nextFloor)
    println(s"floorState: $floorState")
    println(s"newFloorState: $newFloorState")
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

  println(input)

  val result = processDelivery(input.head, Floors(0, 0, 0, 0)).currentFloor
  println(s"result: $result")

  val allResults = input.map(i => processDelivery(i, Floors(0, 0, 0, 0)))
  println(s"all results: $allResults")

//  Part 2


}
