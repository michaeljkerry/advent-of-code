package Year2016

import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day8 extends App {
  val input = fromResource("Year2016/Day8Test.txt").getLines().toList
  println(input)

  sealed trait InstructionType
  case object Rect extends InstructionType
  case object RotateColumn extends InstructionType
  case object RotateRow extends InstructionType

  case class Instruction(instructionType: InstructionType, x: Int, y: Int, amount: Int)
  case class Pixel(x: Int, y: Int, on: Boolean)

  def parseInstruction(text: String): Instruction = {
    val instructionType = text match {
      case t if t.contains("rect") => Rect
      case t if t.contains("rotate column") => RotateColumn
      case t if t.contains("rotate row") => RotateRow
    }

    val rectRegex: Regex = raw"rect\s*(\d*)x(\d*)".r
    val rotateColumnRegex: Regex = raw"rotate column\s*x=(\d*)\s*by\s*(\d*)".r
    val rotateRowRegex: Regex = raw"rotate row\s*y=(\d*)\s*by\s*(\d*)".r

    instructionType match {
      case Rect =>
        val numbers = rectRegex.findAllIn(text)
        Instruction(Rect, numbers.group(1).toInt, numbers.group(2).toInt, 0)
      case RotateColumn =>
        val numbers = rotateColumnRegex.findAllIn(text)
        Instruction(RotateColumn, numbers.group(1).toInt, 0, numbers.group(2).toInt)
      case RotateRow =>
        val numbers = rotateRowRegex.findAllIn(text)
        Instruction(RotateRow, 0, numbers.group(1).toInt, numbers.group(2).toInt)
    }
  }

  println(input.map(parseInstruction))

  val screen = for {
    x <- 0 to 49
    y <- 0 to 5
  } yield Pixel(x, y, false)

  println(screen)

  assert(parseInstruction("rect 35x1") == Instruction(Rect, 35, 1, 0))
  assert(parseInstruction("rotate column x=15 by 6") == Instruction(RotateColumn, 15, 0, 6))
}
