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
    def close(other: Position): Boolean = 
      Math.abs(this.x - other.x) <= 1 && Math.abs(this.y - other.y) <= 1
    def follow(other: Position): Boolean = 
      Math.abs(this.x - other.x) == 0 || Math.abs(this.y - other.y) == 0
    def up: Position = Position(this.x, this.y + 1)
    def down: Position = Position(this.x, this.y - 1)
    def left: Position = Position(this.x - 1, this.y)
    def right: Position = Position(this.x + 1, this.y)

  object Position:
    val zero = Position(0, 0)

  case class Rope(head: Position, tail: Position):

    def up: Rope =
      val newHead = head.up
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, tail.up)
      else
        Rope(newHead, Position(head.x, tail.y + 1))

    def down: Rope =
      val newHead = head.down
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, tail.down)
      else
        Rope(newHead, Position(head.x, tail.y - 1))

    def left: Rope =
      val newHead = head.left
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, tail.left)
      else
        Rope(newHead, Position(tail.x - 1, head.y))

    def right: Rope =
      val newHead = head.right
      if (newHead overlap tail)
        Rope(newHead, tail)
      else if (newHead close tail)
        Rope(newHead, tail)
      else if (newHead follow tail)
        Rope(newHead, tail.right)
      else
        Rope(newHead, Position(tail.x + 1, head.y))

  object Rope:
    val start = Rope(Position.zero, Position.zero)

  case class LongRope(knots: List[Position]):
    val head = knots.head
    val tail = knots.tail
    val last = knots.last

    def up: LongRope = LongRope(tail.foldLeft(List(head.up)) {
      case (state, current) => 
        val next = if (state.last overlap current)
          current
        else if (state.last close current)
          current
        else if (state.last follow current)
          current.up
        else
          Position(state.last.x, current.y + 1)
        state :+ next
    })

    def down: LongRope = LongRope(tail.foldLeft(List(head.down)) {
      case (state, current) => 
        val next = if (state.last overlap current)
          current
        else if (state.last close current)
          current
        else if (state.last follow current)
          current.down
        else
          Position(state.last.x, current.y - 1)
        state :+ next
    })

    def left: LongRope = LongRope(tail.foldLeft(List(head.left)) {
      case (state, current) => 
        val next = if (state.last overlap current)
          current
        else if (state.last close current)
          current
        else if (state.last follow current)
          current.left
        else
          Position(current.x - 1, state.last.y)
        state :+ next
    })

    def right: LongRope = LongRope(tail.foldLeft(List(head.right)) {
      case (state, current) => 
        val next = if (state.last overlap current)
          current
        else if (state.last close current)
          current
        else if (state.last follow current)
          current.right
        else
          Position(current.x + 1, state.last.y)
        state :+ next
    })

  object LongRope:
    def apply(p0: Position, p1: Position, p2: Position, p3: Position, p4: Position, 
              p5: Position, p6: Position, p7: Position, p8: Position, p9: Position): LongRope =
      LongRope(List(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9))
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
    result.groupBy(_.last).size

@main def main: Unit =
  val input = Source.fromFile("input/day9.txt").getLines().mkString("\n")
  println(Day9.part1(input))
  println(Day9.part2(input))

