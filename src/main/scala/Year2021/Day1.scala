package Year2021

import scala.io.Source.fromResource

object Day1 extends App {
  val input = fromResource("Year2021/Day1.txt")
    .getLines()
    .toList
    .map(_.toInt)

  val part1Answer = input
    .foldLeft((0, 0)) { (state, next) => {
      if (next > state._2) (state._1 + 1, next) else (state._1, next)
    }
    }
    ._1 - 1

  println(part1Answer)

  val part2Answer = input
    .sliding(3)
    .foldLeft((0, 0)) { (state, next) => {
      val windowSum = next.sum
      if (windowSum > state._2) (state._1 + 1, windowSum) else (state._1, windowSum)
    }
    }._1 - 1

  println(part2Answer)
}
