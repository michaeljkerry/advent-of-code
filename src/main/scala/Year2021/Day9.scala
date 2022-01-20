package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day9 extends App {

  case class HeightPoint(xIndex: Int, yIndex: Int, height: Int, lowPoint: Boolean)
  case class Basin(heightPoints: Set[HeightPoint])

  val positions = fromResource("Year2021/Day9.txt")
    .getLines()
    .toList
    .map(_.toList
      .map(_.toString)
      .map(_.toInt)
      .zipWithIndex
      .map(h => HeightPoint(h._2, -1, h._1, false)))
    .zipWithIndex
    .flatMap(h => h._1.map(_.copy(yIndex = h._2)))

  def getNeighbours(point: HeightPoint): List[HeightPoint] =
    positions
      .filter(p => (p.xIndex == point.xIndex && p.yIndex == point.yIndex - 1) || (p.xIndex == point.xIndex && p.yIndex == point.yIndex + 1) || (p.xIndex == point.xIndex - 1  && p.yIndex == point.yIndex) || (p.xIndex == point.xIndex + 1  && p.yIndex == point.yIndex))

  val lowestPoints = positions
    .map{ p => {
    val neighbours = getNeighbours(p)
    val lowestHeightOfNeighbours = neighbours.minBy(_.height).height
    if (p.height < lowestHeightOfNeighbours) p.copy(lowPoint = true) else p
  }}
    .filter(_.lowPoint)

  val part1Answer = lowestPoints
    .map(_.height + 1)
    .sum

  println(part1Answer)

  def getBasinNeighbours(point: HeightPoint): List[HeightPoint] =
    getNeighbours(point)
      .filter(n => (n.height > point.height) && (n.height != 9))

  def updateBasin(basin: Basin): Basin = {
    val basinNeighbours = basin.heightPoints.flatMap(getBasinNeighbours)
    Basin(basin.heightPoints ++ basinNeighbours)
  }

  @tailrec
  def runBasin(basin: Basin): Basin = {
    val currentBasinSize = basin.heightPoints.size
    val updatedBasin = updateBasin(basin)
    val updatedBasinSize = updatedBasin.heightPoints.size
    if (currentBasinSize == updatedBasinSize) basin
    else runBasin(updatedBasin)
  }

  val basins = lowestPoints.map(p => runBasin(Basin(Set(p))))

  val part2Answer = basins
    .map(_.heightPoints.size)
    .sorted
    .reverse
    .take(3)
    .product

  println(part2Answer)

}
