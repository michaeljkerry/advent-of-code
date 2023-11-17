package Year2015

import scala.io.Source.fromResource

object Day2 extends App {

  val input = fromResource("Year2015/Day2.txt")
    .getLines()
    .toList
    .map(_.split('x').toList.map(_.toInt))

//  println(input)

  def calculatePaperRequired(l: Int, w: Int, h: Int): Int = {
    val side1 = l * w
    val side2 = l * h
    val side3 = w * h
    (2 * side1) + (2 * side2) + (2 * side3) + List(side1, side2, side3).min
  }

  assert(calculatePaperRequired(2, 3, 4) == 58)
  assert(calculatePaperRequired(1, 1, 10) == 43)

  val resultPart1 = input.foldLeft(0) { (acc, next) =>
    {
      val p = calculatePaperRequired(next(0), next(1), next(2))
      acc + p
    }
  }

  println(resultPart1)

  def calculateRibbonRequired(l: Int, w: Int, h: Int): Int = {
    val sides  = List(l, w, h).sorted.take(2)
    val ribbon = sides(0) + sides(0) + sides(1) + sides(1)
    val bow = l * w * h
    ribbon + bow
  }

  assert(calculateRibbonRequired(2, 3, 4) == 34)
  assert(calculateRibbonRequired(1, 1, 10) == 14)

  val resultPart2 = input.foldLeft(0) { (acc, next) =>
    {
      acc + calculateRibbonRequired(next(0), next(1), next(2))
    }
  }
  println(resultPart2)
}
