package Year2021

import monocle.{Focus, Traversal}

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day4 extends App {

  val input = fromResource("Year2021/Day4.txt").mkString.split("\n")
  val calledNumbers = input.head.split(",").toList.map(_.toInt)
  val boardData = input
    .tail
    .mkString("\n")
    .split("\n\n")
    .filterNot(_.trim.isEmpty)
    .map {
      _.split("\n")
        .map(_.split("\\s+").toList.filterNot(_.isEmpty).map(n => NumberState(n.toInt, false)))
        .toList
        .filterNot(_.isEmpty)
        .map(Row)
    }
    .toList
    .map(Board)

  case class Board(rows: List[Row])

  case class Row(numberStates: List[NumberState])

  case class NumberState(number: Int, called: Boolean)

  case class BingoGame(boards: Set[Board], lastWinningBoard: Option[(Board, Int)])

  val rows = Focus[Board](_.rows)
  val numberStates = Focus[Row](_.numberStates)
  val number = Focus[NumberState](_.number)
  val called = Focus[NumberState](_.called)
  val allNumberStates = rows.each.andThen(numberStates).each

  def updateBoard(calledNumber: Int, board: Board, allNumberStates: Traversal[Board, NumberState]): Board = {
    allNumberStates.
      modify {
        ns => {
          if (ns.number == calledNumber) called.replace(true)(ns) else ns
        }
      }(board)
  }

  def checkForBoardsWithCompletedRows(boards: Set[Board]): Set[Board] =
    boards
      .collect { case b if (b.rows.collect { case (r) if (r.numberStates.forall(_.called)) => r }).nonEmpty => b }

  def checkForBoardsWithCompletedColumns(boards: Set[Board]): Set[Board] =
    boards
      .collect { case b if (b.rows.map(_.numberStates).transpose.collect { case (ns) if (ns.forall(_.called)) => ns }).nonEmpty => b }

  def completeRound(boards: Set[Board], calledNumber: Int, allNumberStates: Traversal[Board, NumberState]): Set[Board] = {
    val updatedBoards = boards.map(b => updateBoard(calledNumber, b, allNumberStates))
    val maybeWinningRowBoards = checkForBoardsWithCompletedRows(updatedBoards)
    val maybeWinningColumnBoards = checkForBoardsWithCompletedColumns(updatedBoards)
    (maybeWinningRowBoards, maybeWinningColumnBoards) match {
      case (rows, _) if rows.nonEmpty => maybeWinningRowBoards
      case (_, columns) if columns.nonEmpty => maybeWinningColumnBoards
      case _ => updatedBoards
    }
  }

  def completeRoundRemovingWinningBoards(gameState: BingoGame, calledNumber: Int, allNumberStates: Traversal[Board, NumberState]): BingoGame = {
    val updatedBoards = gameState.boards.map(b => updateBoard(calledNumber, b, allNumberStates))
    val maybeWinningRowBoards = checkForBoardsWithCompletedRows(updatedBoards)
    val maybeWinningColumnBoards = checkForBoardsWithCompletedColumns(updatedBoards)

    val winningBoardsAfterRound = maybeWinningRowBoards ++ maybeWinningColumnBoards
    val boardsLeft = updatedBoards -- winningBoardsAfterRound

    if (gameState.boards.toList.length == 1 && winningBoardsAfterRound.toList.length == 1) BingoGame(boardsLeft, Some((winningBoardsAfterRound.head, calledNumber)))
    else BingoGame(boardsLeft, None)
  }

  @tailrec
  def runBingo(boards: Set[Board], calledNumbers: List[Int], allNumberStates: Traversal[Board, NumberState]): (Board, Int) = {
    calledNumbers match {
      case n :: rest => {
        val updatedBoards = completeRound(boards, n, allNumberStates)
        if (updatedBoards.toList.length == 1) (updatedBoards.head, n) else runBingo(updatedBoards, rest, allNumberStates)
      }
      case Nil => (boards.head, 99)
    }
  }

  @tailrec
  def runBingoLastBoardWins(gameState: BingoGame, calledNumbers: List[Int], allNumberStates: Traversal[Board, NumberState]): BingoGame = {
    if (gameState.lastWinningBoard.nonEmpty) gameState
    else {
      calledNumbers match {
        case n :: rest => {
          val newGameState = completeRoundRemovingWinningBoards(gameState, n, allNumberStates)
          runBingoLastBoardWins(newGameState, rest, allNumberStates)
        }
        case Nil => gameState
      }
    }
  }

  val winningBoard = runBingo(boardData.toSet, calledNumbers, allNumberStates)
  val unmarkedNumberSum = allNumberStates.getAll(winningBoard._1).filterNot(_.called).map(_.number).sum
  val part1Answer = unmarkedNumberSum * winningBoard._2
  println(part1Answer)

  val finalGameState = runBingoLastBoardWins(BingoGame(boardData.toSet, None), calledNumbers, allNumberStates)
  val lastWinningBoard = finalGameState.lastWinningBoard.get._1
  val calledNumber = finalGameState.lastWinningBoard.get._2
  val lastBoardUnmarkedNumberSum = allNumberStates.getAll(lastWinningBoard).filterNot(_.called).map(_.number).sum
  val part2Answer = lastBoardUnmarkedNumberSum * calledNumber
  println(part2Answer)

}
