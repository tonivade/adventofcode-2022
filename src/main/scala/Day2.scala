package day2

import scala.io.Source

object Day2:
  def part1(input: List[String]): Int =
    val game = input.map(_.split(" "))
    // A = X = rock
    // B = Y = paper
    // C = Z = scissors
    // rock defeats scissors
    // scissors defeats pager
    // paper defeats rock
    val lost = 0
    val draw = 3
    val win = 6
    val rock = 1
    val paper = 2
    val scissors = 3
    game.map {
      case Array("A", "X") => draw + rock
      case Array("A", "Y") => win + paper
      case Array("A", "Z") => lost + scissors
      case Array("B", "X") => lost + rock
      case Array("B", "Y") => draw + paper
      case Array("B", "Z") => win + scissors
      case Array("C", "X") => win + rock
      case Array("C", "Y") => lost + paper
      case Array("C", "Z") => draw + scissors
    }.sum

@main def main: Unit =
  println(Day2.part1(Source.fromFile("input/day2.txt").getLines().toList))
