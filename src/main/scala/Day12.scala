package day12

import scala.io.Source

object Day12:
  def part1(input: String): Int = ???
  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
  println(Day12.part2(input))

