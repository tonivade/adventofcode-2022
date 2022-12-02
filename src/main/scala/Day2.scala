package day2

import scala.io.Source

object Day2:

  def part1(input: List[String]): Int =
    val game = input.map(_.split(" "))
    // A = X = rock
    // B = Y = paper
    // C = Z = scissors
    // rock defeats scissors
    // scissors defeats paper
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

  def part2(input: List[String]): Int = 
    val game = input.map(_.split(" "))
    // A = rock, X = you need to lose
    // B = paper, Y = you need to draw
    // C = scissors, Z = you need to win
    // rock defeats scissors
    // scissors defeats paper
    // paper defeats rock
    val lost = 0
    val draw = 3
    val win = 6
    val rock = 1
    val paper = 2
    val scissors = 3
    game.map {
      case Array("A", "X") => scissors + lost
      case Array("A", "Y") => rock + draw
      case Array("A", "Z") => paper + win
      case Array("B", "X") => rock + lost
      case Array("B", "Y") => paper + draw
      case Array("B", "Z") => scissors + win
      case Array("C", "X") => paper + lost
      case Array("C", "Y") => scissors + draw
      case Array("C", "Z") => rock + win
    }.sum

@main def main: Unit =
  val input = Source.fromFile("input/day2.txt").getLines().toList
  println(Day2.part1(input))
  println(Day2.part2(input))
