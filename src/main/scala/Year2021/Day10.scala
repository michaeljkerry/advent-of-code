package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

//  {([(<{}[<>[]}>{[]{[(<()>
//  {([(<{}[<>[] -> } is wrong should be ]

object Day10 extends App {
  val input = fromResource("Year2021/Day10.txt")
    .getLines()
    .toList
    .map(_.toList)

  case class Line(characters: List[Char], charsToProcess: List[Char], pendingOpeningSymbols: List[Char], syntaxErrors: List[Char])

  val closingSymbols = Map(
    ')' -> '(',
    '}' -> '{',
    ']' -> '[',
    '>' -> '<'
  )

  val openingSymbols = Map(
    '(' -> ')',
    '{' -> '}',
    '[' -> ']',
    '<' -> '>'
  )

  val syntaxErrorScore = Map(
    ')' -> 3,
    '}' -> 1197,
    ']' -> 57,
    '>' -> 25137
  )

  val completionScore = Map(
    ')' -> 1,
    '}' -> 3,
    ']' -> 2,
    '>' -> 4
  )

  def isOpeningSymbol(c: Char): Boolean = "({[<".contains(c)
  def isClosingSymbol(c: Char): Boolean = ")}]>".contains(c)

  @tailrec
  def processLine(line: Line): Line = {
    line.charsToProcess match {
      case currentSymbol :: rest =>
        if (isClosingSymbol(currentSymbol)) {
//          check there is a pending opening symbol
//          what is the last opening symbol?
          val openingSymbol = closingSymbols.getOrElse(currentSymbol, '?')
          val newSyntaxErrors = if (line.pendingOpeningSymbols.head == openingSymbol) line.syntaxErrors else currentSymbol :: line.syntaxErrors
          processLine(Line(line.characters, rest, line.pendingOpeningSymbols.tail, newSyntaxErrors))
        }
        else {
//          opening symbol
          processLine(Line(line.characters, rest, currentSymbol :: line.pendingOpeningSymbols, line.syntaxErrors))
        }

      case Nil => line
    }
  }

  val processedLines = input.map(line => processLine(Line(line, line, List.empty[Char], List.empty[Char])))

  val partitionedLines = processedLines.partition(_.syntaxErrors.isEmpty)

  val part1Answer = partitionedLines
    ._2
    .flatMap(_.syntaxErrors)
    .map(c => syntaxErrorScore.getOrElse(c, 0))
    .sum

  println(part1Answer)

  val lineCompletionScores = partitionedLines
    ._1
    .map(_.pendingOpeningSymbols)
    .map(pos => pos.map(c => openingSymbols.getOrElse(c, '?')))
    .map(cs => cs.map(s => completionScore.getOrElse(s, 0).toLong))
    .map{symbolScores => {
      symbolScores.foldLeft(0L){(total, currentScore) => {
        (total * 5) + currentScore
      }
    }}}
    .sorted
    .zipWithIndex

  val middleIndex = lineCompletionScores.length / 2
  val part2Answer = lineCompletionScores.filter(_._2 == middleIndex).head._1

  println(part2Answer)

}
