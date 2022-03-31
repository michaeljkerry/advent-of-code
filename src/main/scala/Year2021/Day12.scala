package Year2021

import scala.annotation.tailrec
import scala.io.Source.fromResource

object Day12 extends App {
  val connectedCaves = fromResource("Year2021/Day12Test.txt")
    .getLines()
    .toList
    .map(_.split("-").toList)
    .map(l => ConnectedCave(l(0), l(1)))

  println(connectedCaves)
  val caves = connectedCaves.map(_.cave1).toSet ++ connectedCaves.map(_.cave2).toSet
  val cavesMap = caves.foldLeft(Map.empty[String, Set[String]]) { (caveMap, cave) => {
    if (cave == "end") caveMap
    else {
      val connecteds = connectedCaves.filter(cc => cc.cave1 == cave || cc.cave2 == cave)
      val value = {
        connecteds.map(_.cave1).toSet ++ connecteds.map(_.cave2).toSet
      }.filterNot(_ == cave)
        .filterNot(_ == "start")
      caveMap + (cave -> value)
    }
  }
  }

  //    exhaust all from final cave to end
  //    exhaust all from second to last cave to end
  //    exhaust all from third to last cave to end
  //    take another route from start and do the same

//  @tailrec
//  def move(currentCave: String, currentPath: List[String], paths: List[List[String]]): (List[String], List[List[String]]) = {
//    println(currentCave)
//    println(currentPath)
//    val newCavesVisited = currentCave :: currentPath
//
//    if (currentCave == "end") {
////      go back to previous cave
//      move(newCavesVisited(1), currentPath, newCavesVisited :: paths)
//    }
//    else {
//      val options = cavesMap
//        .getOrElse(currentCave, Set.empty)
//        .toList
//        .filterNot(c => c.toCharArray.head.isLower && newCavesVisited.contains(c))
//
//      move(options.head, currentPath, paths)
//
////      routes.foldLeft(Path(List(), List())){(state, route) => {
////        move(route.head, state)
////      }}
////      path
//
////      options match {
////        case head :: rest => {
////          move(head, path.copy(cavesVisited = newCavesVisited))
////        }
////        case Nil => move(newCavesVisited(1), path.copy(previousPaths = newCavesVisited :: path.previousPaths))
////      }
//    }
//  }
//
//  val result = move("start", Path(List.empty[String], List.empty[Path]))
//
//  println(result)

  case class ConnectedCave(cave1: String, cave2: String)

  case class Path(cavesVisited: List[String], previousPaths: List[Path])

}
