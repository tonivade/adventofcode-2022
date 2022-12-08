package day8

class Day8Suite extends munit.FunSuite:

  val input = """30373
                |25512
                |65332
                |33549
                |35390""".stripMargin

  test("day 8 part 1") {
    assertEquals(Day8.part1(input), 21)
  }

//  test("day 8 part 2") {
//    assertEquals(Day8.part2(input), 24933642)
//  }