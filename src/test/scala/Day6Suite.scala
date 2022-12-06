package day6

class Day6Suite extends munit.FunSuite {

  test("day 6 part 1") {
    assertEquals(Day6.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7)
    assertEquals(Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5)
    assertEquals(Day6.part1("nppdvjthqldpwncqszvftbrmjlhg"), 6)
    assertEquals(Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10)
    assertEquals(Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11)
  }

  test("day 6 part 2") {
    assertEquals(Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19)
    assertEquals(Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23)
    assertEquals(Day6.part2("nppdvjthqldpwncqszvftbrmjlhg"), 23)
    assertEquals(Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29)
    assertEquals(Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26)
  }
}