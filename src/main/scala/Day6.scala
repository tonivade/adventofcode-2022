package day6

import scala.io.Source

object Day6:

  def detect(input: String, length: Int): Int =
    input.zipWithIndex.sliding(length)
      .find {
        case array => array.map(_._1).distinct.size == length
      }
      .map(_.last._2 + 1)
      .get

  def part1(input: String): Int = detect(input, 4)

  def part2(input: String): Int = detect(input, 14)

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))
  println(Day6.part2(input))