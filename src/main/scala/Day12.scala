package day12

import scala.io.Source
import scala.collection.mutable.HashMap
import java.util.PriorityQueue

object Day12:

  case class Point(x: Int, y: Int):
    def adjacent: List[Point] = List(up, down, left, right)
    def up: Point = Point(x, y + 1)
    def down: Point = Point(x, y - 1)
    def left: Point = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

  class Node(val point: Point, var parent: Point, var distance: Long) extends Comparable[Node]:
    override def compareTo(other: Node): Int =
      if (this.distance > other.distance) 1
      else if (this.distance < other.distance) -1
      else 0
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
      val queue = PriorityQueue[Node]()
      map.keySet.map {
        case p if (p == start) => Node(start, null, 0)
        case p => Node(p, null, Long.MaxValue)
      }.foreach(queue.add)

      val routes = map.map {
        case (from, _) => (from, moves(from).map {
          to => (to, 1)
        }.toMap)
      }

      val index = HashMap.empty[Point, Node]
      queue.forEach {
        node => index.put(node.point, node)
      }

      while(!queue.isEmpty) {
        val node = queue.remove()
        val edges = routes(node.point)

        edges.foreach {
          (to, cost) =>
            val distance = node.distance + cost
            val other = index(to)
            if (distance < other.distance) {
              other.distance = distance
              other.parent = node.point
              queue.remove(other)
              queue.add(other)
            }
        }
      }

      Map.from(index)(end).distance

    def moves(current: Point): List[Point] =
      val value = map(current)
      val next = if (value == 'S') 'a'
                 else if (value == 'z') 'E'
                 else value + 1
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

