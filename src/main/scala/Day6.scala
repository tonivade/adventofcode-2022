package day6

import scala.io.Source

object Day6:

  def part1(input: String): Int =
    input.zipWithIndex.sliding(4)
      .find {
        case array => array.map(_._1).distinct.size == 4
      }
      .map(_.last._2 + 1)
      .get

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))