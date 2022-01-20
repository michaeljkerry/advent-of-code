package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day3 extends App {
  val input = loadInput("Year2021/Day3.txt")
  //  println(s"input data: ${input}")

  val listOfMaps: List[Map[Char, List[String]]] = input
    .head
    .zipWithIndex
    .map { t => input.groupBy(_.charAt(t._2)) }
    .toList

  val gammaRate: String = listOfMaps
    .map { m => if (m.getOrElse('0', List.empty[Char]).length > m.getOrElse('1', List.empty[Char]).length) "0" else "1" }
    .mkString

  val gammaRateDec = Integer.parseInt(gammaRate, 2)
  val epsilonRate: String = gammaRate.map(x => if (x == '0') '1' else '0')

  val epsilonRateDec = Integer.parseInt(epsilonRate, 2)
  val total = epsilonRateDec * gammaRateDec
  println(s"Part1 answer: ${total}")

  @tailrec
  def runO2Filter(input: List[String], index: Int): List[String] = {
    val zeroesCount = input.groupBy(_.charAt(index)).getOrElse('0', List.empty[Char]).length
    val onesCount = input.groupBy(_.charAt(index)).getOrElse('1', List.empty[Char]).length

    val bit: Char = (zeroesCount, onesCount) match {
      case (zeroes, ones) if zeroes == ones => '1'
      case (zeroes, ones) if zeroes > ones => '0'
      case _ => '1'
    }
    val filteredValues = input.filter(_.charAt(index) == bit)
    if (filteredValues.length == 1) filteredValues
    else runO2Filter(filteredValues, index + 1)
  }

  @tailrec
  def runCO2Filter(input: List[String], index: Int): List[String] = {
    val zeroesCount = input.groupBy(_.charAt(index)).getOrElse('0', List.empty[Char]).length
    val onesCount = input.groupBy(_.charAt(index)).getOrElse('1', List.empty[Char]).length

    val bit: Char = (zeroesCount, onesCount) match {
      case (zeroes, ones) if zeroes == ones => '0'
      case (zeroes, ones) if zeroes > ones => '1'
      case _ => '0'
    }
    val filteredValues = input.filter(_.charAt(index) == bit)
    if (filteredValues.length == 1) filteredValues
    else runCO2Filter(filteredValues, index + 1)
  }

  val o2FilterResult = runO2Filter(input, 0)
  val co2FilterResult = runCO2Filter(input, 0)
  val o2Dec = Integer.parseInt(o2FilterResult.head, 2)
  val co2Dec = Integer.parseInt(co2FilterResult.head, 2)
  val part2Answer = o2Dec * co2Dec

  println(s"Part2 answer: ${part2Answer}")

  def loadInput(filePath: String): List[String] = fromResource(filePath).getLines().toList


}
