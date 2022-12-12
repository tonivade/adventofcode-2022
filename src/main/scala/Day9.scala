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
    def atTop(other: Position): Boolean = other.y > this.y
    def atBottom(other: Position): Boolean = other.y < this.y
    def atLeft(other: Position): Boolean = other.x < this.x
    def atRight(other: Position): Boolean = other.x > this.x
    def up: Position = Position(this.x, this.y + 1)
    def down: Position = Position(this.x, this.y - 1)
    def left: Position = Position(this.x - 1, this.y)
    def right: Position = Position(this.x + 1, this.y)
    def moveTo(other: Position): Position =
      if (other overlap this)
        this
      else if (other close this)
        this
      else if ((other atLeft this) && (other atBottom this))
        this.up.right
      else if ((other atLeft this) && (other atTop this))
        this.down.right
      else if ((other atRight this) && (other atBottom this))
        this.up.left
      else if ((other atRight this) && (other atTop this))
        this.down.left
      else if (other atTop this)
        this.down
      else if (other atBottom this)
        this.up
      else if (other atLeft this)
        this.right
      else
        this.left

  object Position:
    val zero = Position(0, 0)

  case class Rope(head: Position, tail: Position):

    def moveTail(newHead: Position): Rope = Rope(newHead, tail moveTo newHead)

    def up: Rope = moveTail(head.up)
    def down: Rope = moveTail(head.down)
    def left: Rope = moveTail(head.left)
    def right: Rope = moveTail(head.right)

  object Rope:
    val start = Rope(Position.zero, Position.zero)

  case class LongRope(knots: List[Position]):
    val head = knots.head
    val tail = knots.tail
    val last = knots.last

    def moveTail(newHead: Position): LongRope =
      LongRope(tail.foldLeft(List(newHead)) {
        case (state, current) =>
          val next = current moveTo state.last
          state :+ next
      })

    def up: LongRope = moveTail(head.up)
    def down: LongRope = moveTail(head.down)
    def left: LongRope = moveTail(head.left)
    def right: LongRope = moveTail(head.right)

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

