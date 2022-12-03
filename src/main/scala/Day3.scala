package day3

import scala.io.Source

object Day3:
  
  def part1(input: String): Int = 
    input.split("\n").map(_.toList)
      .map(x => x.splitAt(x.size / 2))
      .map {
        case (c1, c2) => c1.toSet.intersect(c2.toSet).head
      }
      .map {
        case x if x.isLower => x.toInt - 'a'.toInt + 1
        case x if x.isUpper => x.toInt - 'A'.toInt + 27
      }.sum
  
  def part2(input: String): Int = 
    input.split("\n").map(_.toList)
      .grouped(3)
      .map {
        case Array(e1, e2, e3) => ((e1.toSet intersect e2.toSet) intersect e3.toSet).head
      }
      .map {
        case x if x.isLower => x.toInt - 'a'.toInt + 1
        case x if x.isUpper => x.toInt - 'A'.toInt + 27
      }.sum

@main def main: Unit =
  val input = Source.fromFile("input/day3.txt").getLines().mkString("\n")
  println(Day3.part1(input))
  println(Day3.part2(input))