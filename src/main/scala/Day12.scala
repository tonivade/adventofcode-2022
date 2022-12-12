package day12

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

object Day12:

  case class Point(x: Int, y: Int):
    def up: Point = Point(x, y + 1)
    def down: Point = Point(x, y - 1)
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

  case class Matrix(map: Map[Point, Char]):
    val start: Point = map.find {
      case (p, 'S') => true
      case _ => false
    }.map(_._1).get
    val end: Point = map.find {
      case (p, 'E') => true
      case _ => false
    }.map(_._1).get

    def search(): List[Point] = Nil

    def move(current: Point, last: Option[Point]): List[Point] =
      val value = map(current)
      val next = if (value == 'S') 'a' else value + 1
      List(current.up, current.down, current.left, current.right)
        .filter(_ != last)
        .map(p => (p, map.get(p)))
        .filter(_._2.isDefined)
        .map(t => (t._1, t._2.get))
        .filter(t => t._2 == value || t._2 == next || t._2 == 'E')
        .map(_._1)

  def part1(input: String): Int =
    val matrix = Matrix(input.split("\n").zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (a, x) => Point(x, y) -> a
      }
    }.toMap)

    println(matrix.move(matrix.start, None))
    println(matrix.move(Point(4, 2), Some(Point(4, 3))))

    0

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
  println(Day12.part2(input))

