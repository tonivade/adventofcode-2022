package day5

import scala.io.Source

object Day5:

  case class Movement(crates: Int, from: Int, to: Int)

  def part1(input: String): String = 
    val Array(crates, movements) = input.split("\n\n")

    var stacks = crates.split("\n")
      .map { x =>
        x.grouped(4).map(_.trim()).map(_.replace("[", "").replace("]", "")).toArray
      }.toArray

    val state = (0 until stacks(0).size).foldLeft(Map.empty[Int, List[String]]) { (map, x) =>
      val r = (0 until stacks.size - 1).foldLeft(List.empty[String]) { (list, y) =>
        stacks(y)(x) :: list
      }.filterNot(_.isEmpty())
      map + (x + 1 -> r)
    }

    val actions = movements.split("\n").map { x =>
      val regex = "move (\\d+) from (\\d+) to (\\d+)".r
      x match {
        case regex(crates, from, to) => Movement(crates.toInt, from.toInt, to.toInt)
      } 
    }

    val result = actions.foldLeft(state) {
      case (s, Movement(crates, from, to)) => 
        (1 to crates).foldLeft(s) {
          case (r, _) => 
            val source = r(from)
            val destination = r(to)

            val a = r + (to -> (destination :+ source.last))
            val b = a + (from -> source.dropRight(1))

            b 
        }
    }

    (1 to result.size).foldLeft("") {
      case (r, x) =>
        r + result(x).last
    }

@main def main: Unit =
  val input = Source.fromFile("input/day5.txt").getLines().mkString("\n")
  println(Day5.part1(input))