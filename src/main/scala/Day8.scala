package day8

import scala.io.Source

object Day8:

  case class Position(x: Int, y: Int)

  case class Matrix(board: Map[Position, Int]):
    val height: Int = board.keySet.map(_._2).max
    val width: Int = board.keySet.map(_._1).max

    def find(task: ((Position, Int)) => Boolean): Map[Position, Int] =
      board.filter(task)

    def visible(position: Position): Boolean = 
      left(position).map(board(_)).forall(_ < board(position)) ||
      right(position).map(board(_)).forall(_ < board(position)) ||
      up(position).map(board(_)).forall(_ < board(position)) ||
      down(position).map(board(_)).forall(_ < board(position))

    def score(position: Position): Int =
      scoreStep(position, left(position)) *
      scoreStep(position, right(position)) *
      scoreStep(position, up(position)) *
      scoreStep(position, down(position))

    def left(position: Position): List[Position] = 
      ((position.x - 1) to 0 by -1).map(Position(_, position.y)).toList
    def right(position: Position): List[Position] =
      ((position.x + 1) to width).map(Position(_, position.y)).toList
    def up(position: Position): List[Position] =
      ((position.y - 1) to 0 by -1).map(Position(position.x, _)).toList
    def down(position: Position): List[Position] =
      ((position.y + 1) to height).map(Position(position.x, _)).toList

    def scoreStep(position: Position, list: List[Position]): Int =
      list.foldLeft((false, 0)) {
        case ((blocked, count), current) => {
          if (!blocked)
            (board(position) <= board(current), count + 1)
          else
            (blocked, count)
        }
      }._2
    
    def mkString: String =
      (0 to height).map { y =>
        (0 to width).map { x =>
          board(Position(x, y))
        }.mkString
      }.mkString("\n")

  def parse(input: String): Matrix =
    Matrix(input.split("\n")
      .zipWithIndex
      .flatMap {
        case (line, y) => line.zipWithIndex.map {
          case (value, x) => (Position(x, y), value.toString().toInt)
        }
      }.foldLeft(Map.empty[Position, Int]) {
        case (map, (position, tree)) => map + (position -> tree)
      })

  def part1(input: String): Int =
    val matrix = parse(input)
    matrix.find {
        case (position, _) => matrix.visible(position)
      }
      .size

  def part2(input: String): Int =
    val matrix = parse(input)
    matrix.board.map {
      case (position, _) => (position, matrix.score(position))
    }.maxBy(_._2)._2

@main def main: Unit =
  val input = Source.fromFile("input/day8.txt").getLines().mkString("\n")
  println(Day8.part1(input))
  println(Day8.part2(input))