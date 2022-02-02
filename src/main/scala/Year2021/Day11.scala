package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day11 extends App {

  val octopuses = fromResource("Year2021/Day11.txt")
    .getLines()
    .toList
    .map(_.toList
      .map(_.toString)
      .map(_.toInt)
      .zipWithIndex
      .map(h => Octopus(h._2, -1, h._1, false)))
    .zipWithIndex
    .flatMap(h => h._1.map(_.copy(yIndex = h._2)))

  def getOctopusNeighbours(octopus: Octopus): List[Octopus] =
    octopuses
      .filter { o => {
//        above
        (o.xIndex == octopus.xIndex && o.yIndex == octopus.yIndex - 1) ||
//        below
          (o.xIndex == octopus.xIndex && o.yIndex == octopus.yIndex + 1) ||
//        left
          (o.xIndex == octopus.xIndex - 1 && o.yIndex == octopus.yIndex) ||
//        right
          (o.xIndex == octopus.xIndex + 1 && o.yIndex == octopus.yIndex) ||
//        above left
          (o.xIndex == octopus.xIndex - 1 && o.yIndex == octopus.yIndex - 1) ||
//        above right
          (o.xIndex == octopus.xIndex + 1 && o.yIndex == octopus.yIndex - 1) ||
//        below left
          (o.xIndex == octopus.xIndex - 1 && o.yIndex == octopus.yIndex + 1) ||
//        below right
          (o.xIndex == octopus.xIndex + 1 && o.yIndex == octopus.yIndex + 1)
      }
      }

  case class Octopus(xIndex: Int, yIndex: Int, energyLevel: Int, flashed: Boolean)

  case class Cavern(octopuses: Set[Octopus], totalFlashes: Int, flashesAfterStep: Int)

  def increaseEnergyLevel(octopuses: Set[Octopus]): Set[Octopus] = octopuses.map(o => o.copy(energyLevel = o.energyLevel + 1))


  def runStep(cavern: Cavern): Cavern = {

    val octopusesResetAfterFlashing = cavern.octopuses.map(o => if (o.flashed) o.copy(flashed = false, energyLevel = 0) else o)

    val energisedOctopuses = increaseEnergyLevel(octopusesResetAfterFlashing)

    val octopusesReadyToFlash = energisedOctopuses.filter(_.energyLevel > 9)

    if (octopusesReadyToFlash.nonEmpty) loop(cavern.copy(octopuses = energisedOctopuses), Set.empty[Octopus])
    else Cavern(energisedOctopuses, cavern.totalFlashes, 0)

  }

  def getNeighbourCoordinates(flashingOctopus: Octopus): Set[(Int, Int)] =
    getOctopusNeighbours(flashingOctopus)
      .map(o => (o.xIndex, o.yIndex))
      .toSet

  @tailrec
  def loop(cavern: Cavern, flashingOctopusesPreviousLoop: Set[Octopus]): Cavern = {

    val octopusesWithFlashesUpdated = cavern.octopuses.map(o => if (o.energyLevel > 9) o.copy(energyLevel = 0, flashed = true) else o)

    val flashedOctopuses = if (flashingOctopusesPreviousLoop.isEmpty) octopusesWithFlashesUpdated.filter(_.flashed) else flashingOctopusesPreviousLoop

    val flashedNeighboursCoordinates = flashedOctopuses.toList.flatMap(getNeighbourCoordinates)

    val octopusesWithFlashedNeighboursUpdated = flashedNeighboursCoordinates.foldLeft(octopusesWithFlashesUpdated){(octopuses, nextCoordinate) => {
      octopuses.map(o => if (o.xIndex == nextCoordinate._1 && o.yIndex == nextCoordinate._2) o.copy(energyLevel = o.energyLevel + 1) else o)
    }}

    val newOctopusesReadyToFlash = octopusesWithFlashedNeighboursUpdated.filter(_.energyLevel > 9)

    if (newOctopusesReadyToFlash.nonEmpty)
      loop(Cavern(octopusesWithFlashedNeighboursUpdated, cavern.totalFlashes, 0), newOctopusesReadyToFlash)
    else {
      val flashesAfterStep = octopusesWithFlashedNeighboursUpdated.count(_.flashed)
      Cavern(octopusesWithFlashedNeighboursUpdated, cavern.totalFlashes + flashesAfterStep, flashesAfterStep)
    }
  }

  val cavernAfter100Steps = (1 to 100).foldLeft(Cavern(octopuses.toSet, 0, 0)){ (cavern, _) => runStep(cavern)}

  val part1Answer = cavernAfter100Steps.totalFlashes
  println(part1Answer)

  val stateAfterSteps = (1 to 220).foldLeft((Cavern(octopuses.toSet, 0, 0), Map.empty[Int, Int])){ (state, step) => {
    val cavernAfterStep = runStep(state._1)
    val newMap = state._2 + (cavernAfterStep.flashesAfterStep -> step)
    (cavernAfterStep, newMap)
  }}

  val part2Answer = stateAfterSteps._2.get(100)
  println(part2Answer)

}
