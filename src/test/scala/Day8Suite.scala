package day8

class Day8Suite extends munit.FunSuite:

  import Day8._

  val input = """30373
                |25512
                |65332
                |33549
                |35390""".stripMargin
  
  test("matrix parsed OK") {
    val matrix = parse(input)
    assertEquals(matrix.board(Position(4, 1)), 2)
    assertEquals(matrix.board(Position(3, 1)), 1)
    assertEquals(matrix.board(Position(2, 1)), 5)
    assertEquals(matrix.board(Position(1, 1)), 5)
    assertEquals(matrix.board(Position(0, 1)), 2)
  }
  
  test("left right up down OK") {
    val matrix = parse(input)
    assertEquals(matrix.left(Position(2, 1)), List(Position(1, 1), Position(0, 1)))
    assertEquals(matrix.right(Position(2, 1)), List(Position(3, 1), Position(4, 1)))
    assertEquals(matrix.up(Position(2, 1)), List(Position(2, 0)))
    assertEquals(matrix.down(Position(2, 1)), List(Position(2, 2), Position(2, 3), Position(2, 4)))
  }

  test("day 8 part 1") {
    assertEquals(part1(input), 21)
  }

  test("day 8 part 2") {
    assertEquals(part2(input), 8)
  }

  test("score of (2,1)") {
    val matrix = parse(input)
    val result = matrix.score(Position(2, 1))
    assertEquals(result, 4)
  }

  test("score of (2,3)") {
    val matrix = parse(input)
    val result = matrix.score(Position(2, 3))
    assertEquals(result, 8)
  }

  test("score of (3,2)") {
    val matrix = parse(input)
    val result = matrix.score(Position(3, 2))
    assertEquals(result, 2)
  }