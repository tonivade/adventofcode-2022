package day12

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object Day12:

  case class Point(x: Int, y: Int):
    def adjacent: List[Point] = List(up, down, left, right)
    def up: Point = Point(x, y + 1)
    def down: Point = Point(x, y - 1)
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

  class Node(val point: Point, var parent: Point, var distance: Long):
    override def toString(): String = s"point=$point, parent=$parent, distance=$distance"

  case class Matrix(map: Map[Point, Char]):
    val start: Point = map.find {
      case (p, 'S') => true
      case _ => false
    }.map(_._1).get
    val end: Point = map.find {
      case (p, 'E') => true
      case _ => false
    }.map(_._1).get

    def search: Long =
      val queue = ListBuffer.empty[Node]
      queue.append(Node(start, null, 0))
      val index = HashMap.empty[Point, Node]
      queue.foreach {
        node => index.put(node.point, node)
      }

      while(!queue.isEmpty) {
        val current = queue.remove(0)

        val adjacent = moves(current.point)

        adjacent.foreach {
          to => if (!index.contains(to))
            val next = Node(to, current.point, current.distance + 1)
            index.put(to, next)
            queue.append(next)
        }
      }

      println(map.size)
      println(index.size)
      index.foreach(println)

      Map.from(index)(end).distance

    def moves(current: Point): List[Point] =
      val value = map(current)
      if (value == 'E')
        Nil
      else
        val next = if (value == 'S') 'a'
                   else if (value == 'z') 'E'
                   else (value + 1).toChar
        current.adjacent
          .map(p => (p, map.get(p)))
          .filter(_._2.isDefined)
          .map(t => (t._1, t._2.get))
          .filter(t => t._2 == value || t._2 == next)
          .map(_._1)

  def part1(input: String): Long =
    val matrix = Matrix(input.split("\n").zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (a, x) => Point(x, y) -> a
      }
    }.toMap)

    matrix.search

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
//  println(Day12.part2(input))

