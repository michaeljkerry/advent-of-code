package Year2021

import scala.io.Source.fromResource

object Day8 extends App {
  val input = fromResource("Year2021/Day8Test.txt").mkString.split("\\|").toList.toString
  println(input)
}
