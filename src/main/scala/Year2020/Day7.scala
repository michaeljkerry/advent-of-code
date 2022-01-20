package Year2020

import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day7 extends App {
  val input = loadInput("Year2020/Day7.txt")
  val bagPattern: Regex = "(.*) bags contain (.*)".r
  val containingBagPattern: Regex = "\\d (.*) ".r

  val parsedInput = for {
    line <- input
    holdingBag <- bagPattern.findFirstMatchIn(line).map(_.group(1))
    containingBagsToSplit <- bagPattern.findFirstMatchIn(line).map(_.group(2))
    containingBags = containingBagsToSplit.split(",").toList.flatMap(s => containingBagPattern.findFirstMatchIn(s).map(_.group(1)))
  } yield (holdingBag, containingBags)

  val bagMap = parsedInput.toMap

  println(bagMap)

  def loadInput(filePath: String): List[String] = fromResource(filePath).getLines().toList

  def getKeysContainingValues(bagMap: Map[String, List[String]], accumulator: Set[String]): Set[String] = {
    val bagsContainingBags = bagMap.collect { case (k, v) if v.toSet.intersect(accumulator).nonEmpty => k }
    val output = bagsContainingBags.toSet
    //    println(s"bagsContainingBags: ${bagsContainingBags.toList.length}")
    //    println(s"output: ${output.toList.length}")
    output
  }

  @tailrec
  def getTotal(allBags: Map[String, List[String]], valuesInputSet: Set[String], iteration: Int): Int = {
    require(iteration < 100)
    val bagKeys = getKeysContainingValues(allBags, valuesInputSet)
    println(s"iteration ${iteration} - bagKeys: ${bagKeys} - length: ${bagKeys.toList.length}, valuesInputSet: ${valuesInputSet}")
    if (bagKeys.toList.length == valuesInputSet.toList.length - 1) bagKeys.toList.length
    else getTotal(allBags, bagKeys ++ valuesInputSet, iteration + 1)
  }

  val answer = getTotal(bagMap, Set("shiny gold"), 1)
  println(answer)
}
