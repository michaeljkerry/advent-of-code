package Year2016

import scala.annotation.tailrec
import scala.io.Source.fromResource
import scala.util.matching.Regex

object Day7 extends App {
  val ipAdresses = fromResource("Year2016/Day7.txt")
    .getLines()
    .toList
    .map(parseIpAdress)

  case class IpAddress(hypernets: List[String], supernets: List[String])

  def parseIpAdress(line: String): IpAddress = {
    val hypernetRegex: Regex = raw"\[(.*?)\]".r
    val supernetRegex: Regex = raw"\w*[^''\]\[]".r
    val hypernets = hypernetRegex.findAllIn(line).toList.map(_.replaceAll("[\\[\\]]", ""))
    val supernets = supernetRegex.findAllIn(line).toList.filterNot(hypernets.contains)
    IpAddress(hypernets, supernets)
  }

  @tailrec
  def hasABBA(text: String): Boolean = {
    val groupedChars = text.toList.sliding(2, 2).toList
    groupedChars match {
      case head :: next :: Nil => compareTwo(head, next)
      case head :: tail => if (compareTwo(head, tail.head)) true else hasABBA(text.drop(1))
      case Nil => false
    }
  }

  def compareTwo(list1: List[Char], list2: List[Char]): Boolean = {
    (list1.mkString == list2.reverse.mkString) && list1(0) != list1(1)
  }

  val part1Answer = ipAdresses.count(ip => ip.hypernets.forall(h => !hasABBA(h)) && ip.supernets.exists(s => hasABBA(s)))
  println(part1Answer)


  def supportsSSL(ipAddress: IpAddress): Boolean = {
    val aba = ipAddress.supernets.flatMap(abas).map(_.mkString)
    val bab = aba.map(makeBAB)
    bab.exists(b => ipAddress.hypernets.exists(h => h.contains(b)))
  }

  def abas(text: String): List[List[Char]] = {
    text.toList.sliding(3).toList.map(isABA).filter(_._1).map(_._2)
  }

  def compareThree(list1: List[Char], list2: List[Char]): Boolean = {
    (list1.mkString == list2.reverse.mkString) && list1(0) != list1(1)
  }

  def isABA(chars: List[Char]): (Boolean, List[Char]) = {
    (chars(0) == chars(2) && chars(0) != chars(1), chars)
  }

  def makeBAB(aba: String): String = {
    val chars = aba.toList
    List(chars(1), chars(0), chars(1)).mkString
  }

  val part2Answer = ipAdresses.map(supportsSSL).count(b => b)
  println(part2Answer)


  assert(parseIpAdress("pjvdfpsdlampeztecfq[lpqshzeegwiouas]nwxqaoryigyvbby[iiddsczjoxentwv]weexunkmtaaufurjz[meywmosucyrxzlgxi]huqfmfpxdmcmqfk") == IpAddress(List("lpqshzeegwiouas", "iiddsczjoxentwv", "meywmosucyrxzlgxi"), List("pjvdfpsdlampeztecfq", "nwxqaoryigyvbby", "weexunkmtaaufurjz", "huqfmfpxdmcmqfk")))
  assert(hasABBA("abba"))
  assert(hasABBA("ioxxoj"))
  assert(!hasABBA("aaaa"))
  assert(isABA(List('x', 'y', 'x')) == (true, List('x', 'y', 'x')))
  assert(isABA(List('x', 'x', 'x')) == (false, List('x', 'x', 'x')))
  assert(makeBAB("xyx") == "yxy")
}
