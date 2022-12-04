package day4

import scala.io.Source

object Day4:

  def toRanges(input: String): Array[(Set[Int], Set[Int])] =
    input.split("\n").map(_.split(","))
      .map {
        case Array(r1, r2) => (r1.split("-"), r2.split("-"))
      }
      .map {
        case (Array(a1, a2), Array(b1, b2)) => ((a1.toInt to a2.toInt).toSet, (b1.toInt to b2.toInt).toSet)
      }

  def part1(input: String): Int =
    toRanges(input).filter {
      case (r1, r2) => (r1 intersect r2) == r2 || (r1 intersect r2) == r1
    }.size

  def part2(input: String): Int =
    toRanges(input).filterNot {
        case (r1, r2) => (r1 intersect r2).isEmpty
      }.size

@main def main: Unit =
  val input = Source.fromFile("input/day4.txt").getLines().mkString("\n")
  println(Day4.part1(input))
  println(Day4.part2(input))