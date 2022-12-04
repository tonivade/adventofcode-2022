package day4

class Day4Suite extends munit.FunSuite {
  val input = """2-4,6-8
                |2-3,4-5
                |5-7,7-9
                |2-8,3-7
                |6-6,4-6
                |2-6,4-8""".stripMargin

  test("day 4 part 1") {
    assertEquals(Day4.part1(input), 2)
  }

  test("day 4 part 2") {
    assertEquals(Day4.part2(input), 4)
  }
}