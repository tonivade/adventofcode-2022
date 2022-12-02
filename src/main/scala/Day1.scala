package day1

import scala.io.Source

object Day1:

  def part1(lines: List[String]): Int =
    lines.mkString(",").split(",,").map(_.split(",").map(_.toInt).sum).max

  def part2(lines: List[String]): Int =
    lines.mkString(",").split(",,").map(_.split(",").map(_.toInt).sum).sorted.reverse.take(3).sum

@main def main: Unit =
  val input = Source.fromFile("input/day1.txt").getLines.toList
  println(Day1.part1(input))
  println(Day1.part2(input))
