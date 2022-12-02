package day1

class Day1Suite extends munit.FunSuite {

  val lines = """1000
                |2000
                |3000
                |
                |4000
                |
                |5000
                |6000
                |
                |7000
                |8000
                |9000
                |
                |10000""".stripMargin

  test("day 1 part 1") {
    assertEquals(Day1.part1(lines.split("\n").toList), 24000)
  }

  test("day 1 part 2") {
    assertEquals(Day1.part2(lines.split("\n").toList), 45000)
  }
}
