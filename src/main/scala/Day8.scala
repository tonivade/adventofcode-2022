package day8

import scala.io.Source

object Day8:

  opaque type Tree = Int
  case class Position(x: Int, y: Int)

  case class Matrix(board: Map[Position, Tree]):
    def height: Int = board.keySet.map(_._2).max
    def width: Int = board.keySet.map(_._1).max
    def foreach(task: (Position, Tree) => Unit): Unit =
      board.foreachEntry(task)

  def part1(input: String): Int =
    val matrix = Matrix(input.split("\n")
      .zipWithIndex
      .flatMap {
        case (line, y) => line.zipWithIndex.map {
          case (value, x) => (Position(x, y), x.toString().toInt)
        }
      }.foldLeft(Map.empty[Position, Tree]) {
        case (map, (position, tree)) => map + (position -> tree)
      })

    1
