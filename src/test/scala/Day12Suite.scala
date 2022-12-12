package day12

import Day12._

class Day12Suite extends munit.FunSuite:

  val input = """Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

  test("Day12 part1") {
    assertEquals(part1(input), 1)
  }

  test("Day12 part2") {
    assertEquals(part2(input), 1)
  }

