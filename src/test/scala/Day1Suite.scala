// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Day1Suite extends munit.FunSuite {
  test("day 1 part 1") {
    var lines = """1000
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
    val result = Day1.part1(lines.split("\n").toList)
    assertEquals(result, 24000)
  }

  test("day 1 part 2") {
    var lines = """1000
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
    val result = Day1.part2(lines.split("\n").toList)
    assertEquals(result, 45000)
  }
}
