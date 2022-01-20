package Year2020

import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day7Part2 extends App {
  val input = loadInput("Year2020/Day7.txt").toList
  val bagPattern: Regex = "(.*) bags contain (.*)".r
  val containingBagPattern: Regex = "(\\d) (.*) ".r

  val bagWithListOfBags = for {
    line <- input
    holdingBag <- bagPattern.findFirstMatchIn(line).map(_.group(1))
    containingBagsToSplit <- bagPattern.findFirstMatchIn(line).map(_.group(2))
    containingBags = containingBagsToSplit.split(",").toList
  } yield Map(holdingBag -> parseContainingBags(containingBags))

  val bagsMap = bagWithListOfBags.flatten.toMap

  println(bagsMap)

  def parseContainingBags(bags: List[String]): Map[String, String] =
    bags.flatMap(bag => Map(containingBagPattern.findFirstMatchIn(bag).map(_.group(2)).getOrElse("empty") -> containingBagPattern.findFirstMatchIn(bag).map(_.group(1)).getOrElse("0"))).toMap

  def loadInput(filePath: String): Iterator[String] = fromResource(filePath).getLines()

  println(bagsMap.get("shiny gold"))

  case class InnerBags(bags: Set[String], totalBags: Int)

  def getInnerBags(allBags: Map[String, Map[String, String]], innerBags: InnerBags, innerBag: String): InnerBags = {
    allBags.get(innerBag) match {
      case Some(value) => InnerBags(value.keys.toSet ++ innerBags.bags, innerBags.totalBags + value.values.head.toInt)
      case None => innerBags
    }
  }

  println(getInnerBags(bagsMap, InnerBags(Set("shiny gold"), 0), "shiny gold"))
}
