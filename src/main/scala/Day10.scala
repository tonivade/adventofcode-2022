package day10

import scala.io.Source

object Day10:

  sealed trait Op
  case object Noop extends Op
  case class Add(x: Int) extends Op

  def part1(input: String): Int = 
    val program = input.split("\n")
      .map(_.split(" "))
      .flatMap {
        case Array(_) => List(Noop)
        case Array(op, i) => List(Noop, Add(i.toInt))
      }
    
    val result = program.foldLeft(List(1)) {
      case (register, Noop) => register :+ register.last
      case (register, Add(x)) => register :+ (register.last + x)
    }

    (result(19) * 20)
    + (result(59) * 60)
    + (result(99) * 100)
    + (result(139) * 140)
    + (result(179) * 180)
    + (result(219) * 220)

@main def main: Unit =
  val input = Source.fromFile("input/day10.txt").getLines().mkString("\n")
  println(Day10.part1(input))
