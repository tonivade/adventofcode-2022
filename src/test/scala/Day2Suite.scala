package day2

class Day2Suite extends munit.FunSuite {

  val lines = """A Y
                |B X
                |C Z""".stripMargin

  test("day 2 part 1") {
    assertEquals(Day2.part1(lines.split("\n").toList), 15)
  }

  test("day 2 part 2") {
    assertEquals(Day2.part2(lines.split("\n").toList), 12)
  }
}
