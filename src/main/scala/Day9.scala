package day9

import scala.io.Source

object Day9:

  sealed trait Movement
  case object Up extends Movement
  case object Down extends Movement
  case object Left extends Movement
  case object Right extends Movement

  case class Position(x: Int, y: Int):
    def overlap(other: Position): Boolean = this == other
    def close(other: Position): Boolean = Math.abs(this.x - other.x) <= 1 && Math.abs(this.y - other.y) <= 1
    def follow(other: Position): Boolean = Math.abs(this.x - other.x) == 0 || Math.abs(this.y - other.y) == 0

  object Position:
    val zero = Position(0, 0)

  case class Rope(head: Position, tail: Position):

    def up: Rope =
      val newHead = Position(head.x, head.y + 1)
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, Position(tail.x, tail.y + 1))
      else
        Rope(newHead, Position(head.x, tail.y + 1))

    def down: Rope =
      val newHead = Position(head.x, head.y - 1)
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, Position(tail.x, tail.y - 1))
      else
        Rope(newHead, Position(head.x, tail.y - 1))

    def left: Rope =
      val newHead = Position(head.x - 1, head.y)
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, Position(tail.x - 1, tail.y))
      else
        Rope(newHead, Position(tail.x - 1, head.y))

    def right: Rope =
      val newHead = Position(head.x + 1, head.y)
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, Position(tail.x + 1, tail.y))
      else
        Rope(newHead, Position(tail.x + 1, head.y))

  object Rope:
    val start = Rope(Position.zero, Position.zero)

  case class LongRope(positions: List[Position]):
    val head = positions.head
    val tail = positions.last
    def up: LongRope = ???
    def down: LongRope = ???
    def left: LongRope = ???
    def right: LongRope = ???

  object LongRope:
    val start = LongRope(List.fill(10)(Position.zero))

  def move(start: Rope, movements: List[Movement]): List[Rope] =
    movements.foldLeft(List(start)) {
      case (state, Up) => state :+ state.last.up
      case (state, Down) => state :+ state.last.down
      case (state, Left) => state :+ state.last.left
      case (state, Right) => state :+ state.last.right
    }

  def moveLong(start: LongRope, movements: List[Movement]): List[LongRope] =
    movements.foldLeft(List(start)) {
      case (state, Up) => state :+ state.last.up
      case (state, Down) => state :+ state.last.down
      case (state, Left) => state :+ state.last.left
      case (state, Right) => state :+ state.last.right
    }

  def parse(input: String): List[Movement] =
    input.split("\n")
      .map(_.split(" "))
      .flatMap {
        case Array("R", i) => List.fill(i.toInt)(Right)
        case Array("L", i) => List.fill(i.toInt)(Left)
        case Array("U", i) => List.fill(i.toInt)(Up)
        case Array("D", i) => List.fill(i.toInt)(Down)
      }
      .toList

  def part1(input: String): Int =
    val movements = parse(input) 
    val result = move(Rope.start, movements)
    result.groupBy(_.tail).size

  def part2(input: String): Int =
    val movements = parse(input) 
    val result = moveLong(LongRope.start, movements)
    result.groupBy(_.tail).size

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))

