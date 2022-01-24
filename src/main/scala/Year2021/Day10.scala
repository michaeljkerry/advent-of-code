package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day10 extends App {
  val input = fromResource("Year2021/Day10.txt")
    .getLines()
    .toList
    .map(_.toList)

  case class Symbol(symbol: Char, count: Int)
  case class Line(characters: List[Char], charsToProcess: List[Char], symbols: List[Symbol], pendingOpeningSymbols: List[Char], syntaxErrors: List[Char])

  val symbols = "{}()[]<>".map(s => Symbol(s, 0)).toList

  def updateSymbols(c: Char, symbols: List[Symbol]): List[Symbol] =
    symbols.map(s => if(s.symbol == c) s.copy(count = s.count + 1) else s)

  def isOpeningSymbol(c: Char): Boolean = "({[<".contains(c)
  def isClosingSymbol(c: Char): Boolean = ")}]>".contains(c)

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

  //  {([(<{}[<>[]}>{[]{[(<()>
  //  {([(<{}[<>[] -> } is wrong should be ]

  @tailrec
  def processLine(line: Line): Line = {
    line.charsToProcess match {
      case currentSymbol :: rest =>
        println(currentSymbol)
        val newSymbols = updateSymbols(currentSymbol, symbols)
        if (isClosingSymbol(currentSymbol)) {
//          check there is a pending opening symbol
//          what is the last opening symbol?
          val openingSymbol = closingSymbols.getOrElse(currentSymbol, '?')
          if (line.pendingOpeningSymbols.head != openingSymbol) {
            println(s"Syntax error: $currentSymbol in $line.charsToProcess expecting ${openingSymbols.getOrElse(line.pendingOpeningSymbols.head, '?')}")
          } else println("Closing symbol correct.")
          val newSyntaxErrors = if (line.pendingOpeningSymbols.head == openingSymbol) line.syntaxErrors else currentSymbol :: line.syntaxErrors
          processLine(Line(line.characters, rest, newSymbols, line.pendingOpeningSymbols.tail, newSyntaxErrors))
        }
        else {
//          opening symbol
          val newSymbols = updateSymbols(currentSymbol, symbols)
          processLine(Line(line.characters, rest, newSymbols, currentSymbol :: line.pendingOpeningSymbols, line.syntaxErrors))
        }

      case Nil => line
    }
  }

  val processedLines = input.map{
    line => {
      println(s"new line start: $line")
      processLine(Line(line, line, symbols, List.empty[Char], List.empty[Char]))
    }
  }

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

  val middleIndex = (lineCompletionScores.length.toDouble / 2.0).floor.toInt
  val part2Answer = lineCompletionScores.filter(_._2 == middleIndex).head._1

  println(part2Answer)

}
