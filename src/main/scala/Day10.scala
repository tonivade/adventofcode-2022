package day10

import scala.io.Source

object Day10:

  sealed trait Op
  case object Noop extends Op
  case class Add(x: Int) extends Op

  def parse(input: String): Array[Op] =
    input.split("\n")
      .map(_.split(" "))
      .flatMap {
        case Array(_) => List(Noop)
        case Array(op, i) => List(Noop, Add(i.toInt))
      }

  def run(program: Array[Op]): List[Int] =
    program.foldLeft(List(1)) {
      case (register, Noop) => register :+ register.last
      case (register, Add(x)) => register :+ (register.last + x)
    }

  def part1(input: String): Int = 
    val program = parse(input)
    val result = run(program)

    (result(19) * 20)
    + (result(59) * 60)
    + (result(99) * 100)
    + (result(139) * 140)
    + (result(179) * 180)
    + (result(219) * 220)

  def part2(input: String): String = 
    val program = parse(input)
    val result = run(program)

    val draw = result.foldLeft("") {
      (state, current) =>
        val position = (state.size % 40) + 1
        println(s"current=$current position=$position")
        if (position >= current && position <= current + 2)
          state + "#"
        else
          state + "."
    }
    
    draw.grouped(40).mkString("\n")

@main def main: Unit =
  val input = Source.fromFile("input/day10.txt").getLines().mkString("\n")
  println(Day10.part1(input))
  println(Day10.part2(input))
