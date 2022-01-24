package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day6 extends App {
  val input = fromResource("Year2021/Day6Test.txt").mkString.split(",").toList
  val inputDigitCounts = input.groupBy(identity).map { case (k, v) => (k, v.size) }
  println(inputDigitCounts)

  val idc = for {
    n <- 0 to 9
  } yield (n, 0)

  val idcMap = idc.toMap

  println(idcMap)

  val startingFish = ???

  //  def calculateFishTimer(fish: List[Short]): List[Short] = {
  //    def getNewFish(fish: Short): Short =
  //      fish match {
  //        case f if f == 0 => 6.toShort
  //        case f => (f - 1).toShort
  //      }
  //    fish.map(getNewFish)
  //  }
  //
  //  @tailrec
  //  def runFish(fish: List[Short], days: List[Short]): Unit = {
  //    println(days.length)
  //    val newFish = calculateFishTimer(fish)
  //    val zeroTimers = fish.count(_ == 0)
  //    val fishWithAdditions = if (zeroTimers > 0)  List.fill(zeroTimers)(8.toShort) ::: newFish else newFish
  //
  //    days match {
  //      case Nil => println(fish.length)
  //      case _ :: tail => runFish(fishWithAdditions, tail)
  //    }
  //  }
  //
  //  val daysToRun = 1 to 256
  //  runFish(input.toList, daysToRun.toList.map(_.toShort))
  //  118
}


