package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day10 extends App {
  val input = fromResource("Year2021/Day10.txt")
    .getLines()
    .toList
    .map(_.toList)
  println(input)

  case class Symbol(symbol: Char, count: Int)
  case class SyntaxErrors(syntaxErrors: List[Char])
  case class OpeningSymbol(symbol: Char, count: Int, pendingClosure: Boolean)
  case class ClosingSymbol(symbol: Char, count: Int, closedAnOpening: Boolean)

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

  @tailrec
  def processLine(line: List[Char], charsToProcess: List[Char], symbols: List[Symbol], pendingOpeningSymbols: List[Char], syntaxErrors: SyntaxErrors): (List[Symbol], SyntaxErrors) = {
//    println(line)
//    println(charsToProcess)
//    println(pendingOpeningSymbols)
    charsToProcess match {
      case currentSymbol :: rest => {
        println(currentSymbol)
        val newSymbols = updateSymbols(currentSymbol, symbols)
        if (isClosingSymbol(currentSymbol)) {
//          check there is a pending opening symbol
//          what is the last opening symbol??
          val openingSymbol = closingSymbols.getOrElse(currentSymbol, '?')
          if (pendingOpeningSymbols.head != openingSymbol) {
            println(s"Syntax error: $currentSymbol in $charsToProcess expecting ${openingSymbols.getOrElse(pendingOpeningSymbols.head, '?')}")
//            processLine(line, rest, newSymbols, pendingOpeningSymbols.tail, SyntaxErrors(currentSymbol :: syntaxErrors.syntaxErrors))
          } else println("OK")
          val newSyntaxErrors = if (pendingOpeningSymbols.head == openingSymbol) syntaxErrors else SyntaxErrors(currentSymbol :: syntaxErrors.syntaxErrors)
          processLine(line, rest, newSymbols, pendingOpeningSymbols.tail, newSyntaxErrors)
        }
        else {
//          opening symbol
          val newSymbols = updateSymbols(currentSymbol, symbols)
          processLine(line, rest, newSymbols, currentSymbol :: pendingOpeningSymbols, syntaxErrors)
        }

      }
      case Nil => (symbols, syntaxErrors)
    }
  }

//  val x = "((((>".toList
//  println(processLine(x, symbols))

  val processFile = input.map{
    line => {
      println(line)
      processLine(line, line, symbols, List.empty[Char], SyntaxErrors(List.empty[Char]))
    }
  }
//  {([(<{}[<>[]}>{[]{[(<()>
//  {([(<{}[<>[] } is wrong should be ]

  println(processFile)
  val syntaxErrorResult = processFile
    .filterNot(_._2.syntaxErrors.isEmpty)
    .map(_._2)
    .flatMap(_.syntaxErrors)
    .map(c => syntaxErrorScore.getOrElse(c, 0))
    .sum
  println(syntaxErrorResult)

//  println(input.map(line => processLine(line, symbols)))

//  val symbols = Map(
//    '(' -> 0,
//    '[' -> 0,
//    '{' -> 0,
//    '<' -> 0,
//    ')' -> 0,
//    ']' -> 0,
//    '}' -> 0,
//    '>' -> 0
//  )
//
//  def updateMap(c: Char): Map[Char, Int] =
//    symbols.get(c) match {
//      case Some(v) => symbols.updated(c, v + 1)
//      case None => symbols.updated(c, 0)
//    }

//  println(updateMap('('))

//  val x = "([])".tapEach(c => if (openSymbols.contains(c)) openSymbols + (c, openSymbols.getOrElse(c, 0) + 1))
//  println(x)

//  val x = "((((".map(updateMap)
//  println(x)

}
