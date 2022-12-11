package day11

import scala.io.Source

object Day11:

  case class Monkey(id: Int, times: Int, items: List[Long], op: Long => Long, test: Long => Boolean, onTrue: Int, onFalse: Int, worry: Int):
    def add(i: List[Long]): Monkey = this.copy(items = items ++ i)
    def step(monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
      val result = items.map {
        case item =>
          val next = op(item) / worry
          if (test(next))
            (onTrue -> next)
          else
            (onFalse -> next)
      }.groupBy(_._1).map {
        case (x, list) => (x, list.map(_._2))
      }
      val merge = result.foldLeft(monkeys) {
        case (map, (id, list)) => map + (id -> map(id).add(list))
      }
      merge + (id -> Monkey(id, times + items.size, List.empty, op, test, onTrue, onFalse, worry))

  def parse(worryLevel: Int)(input: String): Map[Int, Monkey] =
    input.split("\n\n")
      .map {
        _.split("\n") match {
          case Array(monkey, starting, operation, divisible, onTrue, onFalse) =>
            val id = monkey.split(" ")(1).split(":")(0).toInt
            val items = starting.split(":")(1).split(",").map(_.trim()).map(_.toLong).toList
            val opRegex = "new = old ([\\+\\*]) ([\\w\\d]+)".r
            val op: Long => Long = operation.split(":")(1).trim() match {
              case opRegex("+", "old") => (x => x + x)
              case opRegex("*", "old") => (x => x * x)
              case opRegex("+", b) => (x => x + b.toInt)
              case opRegex("*", b) => (x => x * b.toInt)
            }
            val testRegex = "divisible by ([\\d]+)".r
            val test: Long => Boolean = divisible.split(":")(1).trim() match {
              case testRegex(a) => (x => (x % a.toInt) == 0)
            }
            val actionRegex = "throw to monkey ([\\d]+)".r
            val actionTrue = onTrue.split(":")(1).trim() match {
              case actionRegex(a) => a.toInt
            }
            val actionFalse = onFalse.split(":")(1).trim() match {
              case actionRegex(a) => a.toInt
            }
            (id, Monkey(id, 0, items, op, test, actionTrue, actionFalse, worryLevel))
        }
      }.toMap

  def run(rounds: Int)(worry: Int)(input: String): Int =
    val monkeys = parse(worry)(input)

    val result = (0 until rounds).foldLeft(monkeys) {
      case (state1, _) =>
        (0 until monkeys.size).foldLeft(state1) {
          case (state2, i) => state2(i).step(state2)
        }
    }

    result.foreach(println)

    result.values.toList.sortBy(_.times).reverse.take(2).map((_.times)).foldLeft(1)(_ * _)

  def part1(input: String): Int = run(20)(3)(input)
  def part2(input: String): Int = run(10000)(1)(input)

@main def main: Unit =
  val input = Source.fromFile("input/day11.txt").getLines().mkString("\n")
  println(Day11.part1(input))
  println(Day11.part2(input))