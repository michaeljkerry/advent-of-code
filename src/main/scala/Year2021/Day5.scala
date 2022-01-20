package Year2021

import scala.io.Source.fromResource
import scala.annotation.tailrec

object Day5 extends App {

  case class Coordinate(x: Int, y: Int)
  case class CoordinateState(coordinate: Coordinate, crossedCount: Int)
  case class Movement(start: Coordinate, end: Coordinate)
  case class Board(coordinateStates: List[CoordinateState])

  val input = fromResource("Year2021/Day5Test.txt").mkString.split("\n").toList
  val allCoordinates = input.map(line => line.split(" -> ").toList.map(p => p.split(",").toList.map(_.toInt)).map(ps => Coordinate(ps.head, ps.last)))
  val movements = input.map(line => line.split(" -> ").toList.map(p => p.split(",").toList.map(_.toInt)).map(ps => Coordinate(ps.head, ps.last))).map(m => Movement(m.head, m.last))
  val maxX = allCoordinates.flatten.maxBy(_.x).x
  val maxY = allCoordinates.flatten.maxBy(_.y).y
  val boardStates = for {
    xp <- 0 to maxX
    yp <- 0 to maxY
  } yield CoordinateState(Coordinate(xp, yp), 0)
  val board = Board(boardStates.toList)

  val horizontalAndVerticalMovements = movements.filter(m => (m.start.x == m.end.x || m.start.y == m.end.y))
  val finalBoardPart1 = runVents(board, horizontalAndVerticalMovements)
  val pointsAtLeast2OverlapPart1 = finalBoardPart1.coordinateStates.count(_.crossedCount >= 2)
  println(pointsAtLeast2OverlapPart1)

  val finalBoardPart2 = runVentsIncludeDiagonals(board, movements)
  val pointsAtLeast2OverlapPart2 = finalBoardPart2.coordinateStates.count(_.crossedCount >= 2)
  println(pointsAtLeast2OverlapPart2)

  def getCrossingPoints(start: Coordinate, end: Coordinate): List[Coordinate] = {
    val xBy = if (start.x < end.x) 1 else -1
    val yBy = if (start.y < end.y) 1 else -1
    val crossingPoints = for {
      xp <- start.x to end.x by xBy
      yp <- start.y to end.y by yBy
    } yield Coordinate(xp, yp)
    crossingPoints.toList
  }

  def getDiagonalCrossingPoints(start: Coordinate, end: Coordinate): List[Coordinate] = {
    val xBy = if (start.x < end.x) 1 else -1
    val yBy = if (start.y < end.y) 1 else -1
    val xRange = start.x to end.x by xBy
    val yRange = start.y to end.y by yBy
    val zipped = xRange.toList.zip(yRange.toList)
    zipped.map(t => Coordinate(t._1, t._2))
  }

  def updateBoard(board: Board, movement: Movement): Board = {
    val crossingPoints = getCrossingPoints(movement.start, movement.end)
    val newCoordinateStates = board.coordinateStates.map(cs => if (crossingPoints.contains(cs.coordinate)) cs.copy(crossedCount = cs.crossedCount + 1) else cs)
    Board(newCoordinateStates)
  }

  def updateBoardDiagonals(board: Board, movement: Movement): Board = {
    val crossingPoints = getDiagonalCrossingPoints(movement.start, movement.end)
    val newCoordinateStates = board.coordinateStates.map(cs => if (crossingPoints.contains(cs.coordinate)) cs.copy(crossedCount = cs.crossedCount + 1) else cs)
    Board(newCoordinateStates)
  }

  @tailrec
  def runVents(board: Board, movements: List[Movement]): Board = {
    movements match {
      case head :: rest => {
        val newBoard = updateBoard(board, head)
        runVents(newBoard, rest)
      }
      case Nil => board
    }
  }

  @tailrec
  def runVentsIncludeDiagonals(board: Board, movements: List[Movement]): Board = {
    movements match {
      case head :: rest => {
        if (head.start.x == head.end.x || head.start.y == head.end.y) {
          val newBoard = updateBoard(board, head)
          runVentsIncludeDiagonals(newBoard, rest)
        }
        else {
          val newBoard = updateBoardDiagonals(board, head)
          runVentsIncludeDiagonals(newBoard, rest)
        }

      }
      case Nil => board
    }
  }

}
