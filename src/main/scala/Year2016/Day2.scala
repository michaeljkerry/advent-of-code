package Year2016

import scala.io.Source.fromResource

object Day2 extends App {

  val input = fromResource("Year2016/Day2.txt")
    .getLines()
    .toList
    .map(_.toList)

  def move(number: Int, command: Char): Int =
    command match {
      case 'U' => if (List(1,2,3).contains(number)) number else number - 3
      case 'D' => if (List(7,8,9).contains(number)) number else number + 3
      case 'L' => if (List(1,4,7).contains(number)) number else number - 1
      case 'R' => if (List(7,8,9).contains(number)) number else number + 1
    }

  def getLineResult(startNo: Int, commands: List[Char]): Int =
    commands.foldLeft(startNo){(currentNo, command) => move(currentNo, command)}

  val resultPart1 = input.foldLeft((5, List.empty[Int])){ (currentNumber, commands) => {
    val lineResult = getLineResult(currentNumber._1, commands)
    (lineResult, currentNumber._2 :+ lineResult)
  }}

  println(resultPart1._2)

  val movesPart2 = Map(
    '1' -> Map('D' -> '3'),
    '2' -> Map('R' -> '3', 'D' -> '6'),
    '3' -> Map('U' -> '1', 'D' -> '7', 'L' -> '2', 'R' -> '4'),
    '4' -> Map('D' -> '8', 'L' -> '3'),
    '5' -> Map('R' -> '6'),
    '6' -> Map('U' -> '2', 'D' -> 'A', 'L' -> '5', 'R' -> '7'),
    '7' -> Map('U' -> '3', 'D' -> 'B', 'L' -> '6', 'R' -> '8'),
    '8' -> Map('U' -> '4', 'D' -> 'C', 'L' -> '7', 'R' -> '9'),
    '9' -> Map('L' -> '8'),
    'A' -> Map('U' -> '6', 'R' -> 'B'),
    'B' -> Map('U' -> '7', 'D' -> 'D', 'L' -> 'A', 'R' -> 'C'),
    'C' -> Map('U' -> '8', 'L' -> 'B'),
    'D' -> Map('U' -> 'B')
  )

  def movePart2(position: Char, command: Char): Char = {
    val validMoves = movesPart2.getOrElse(position, Map.empty[Char, Char])
    validMoves.get(command) match {
      case Some(newPosition) => newPosition
      case None => position
    }
  }

  def getLineResultPart2(startPosition: Char, commands: List[Char]): Char =
    commands.foldLeft(startPosition){ (currentPosition, command) => movePart2(currentPosition, command)}

  val resultPart2 = input.foldLeft(('5', List.empty[Char])){ (currentPosition, commands) => {
    val lineResult = getLineResultPart2(currentPosition._1, commands)
    (lineResult, currentPosition._2 :+ lineResult)
  }}

  println(resultPart2._2)
}
