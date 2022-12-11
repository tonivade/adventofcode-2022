package day11

class Day11Suite extends munit.FunSuite:

  val input = """Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3
                |
                |Monkey 1:
                |  Starting items: 54, 65, 75, 74
                |  Operation: new = old + 6
                |  Test: divisible by 19
                |    If true: throw to monkey 2
                |    If false: throw to monkey 0
                |
                |Monkey 2:
                |  Starting items: 79, 60, 97
                |  Operation: new = old * old
                |  Test: divisible by 13
                |    If true: throw to monkey 1
                |    If false: throw to monkey 3
                |
                |Monkey 3:
                |  Starting items: 74
                |  Operation: new = old + 3
                |  Test: divisible by 17
                |    If true: throw to monkey 0
                |    If false: throw to monkey 1""".stripMargin

  test("day 11 part 1") {
    assertEquals(Day11.part1(input), 10605L)
  }

  test("day 11 part 2") {
    assertEquals(Day11.run(1)(1)(input), 6L * 4L)
    assertEquals(Day11.run(20)(1)(input), 103L * 99L)
    assertEquals(Day11.run(1000)(1)(input), 5204L * 5192L)
    assertEquals(Day11.part2(input), 52166L * 52013L)
  }
