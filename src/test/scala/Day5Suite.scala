package day5

class Day5Suite extends munit.FunSuite {

  val input = """    [D]    
                |[N] [C]    
                |[Z] [M] [P]
                | 1   2   3 
                |
                |move 1 from 2 to 1
                |move 3 from 1 to 3
                |move 2 from 2 to 1
                |move 1 from 1 to 2""".stripMargin

  test("day 5 part 1") {
    assertEquals(Day5.part1(input), "CMZ")
  }
}