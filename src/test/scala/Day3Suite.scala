package day3

class Day3Suite extends munit.FunSuite {
  val input = """vJrwpWtwJgWrhcsFMMfFFhFp
                |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                |PmmdzqPrVvPwwTWBwg
                |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                |ttgJtRGJQctTZtZT
                |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  test("day 3 part 1") {
    assertEquals(Day3.part1(input), 157)
  }

  test("day 3 part 2") {
    assertEquals(Day3.part2(input), 70)
  }
}