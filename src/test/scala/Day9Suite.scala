package day9

class Day9Suite extends munit.FunSuite:

  import Day9._

  val input = """R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin

  val input2 = """R 5
                 |U 8
                 |L 8
                 |D 3
                 |R 17
                 |D 10
                 |L 25
                 |U 20""".stripMargin

  test("day 9 part 1") {
    assertEquals(part1(input), 13)
  }

  test("head tail overlap") {
    assertEquals(Position(0, 0) overlap Position(0, 0), true)
    assertEquals(Position(1, 0) overlap Position(0, 0), false)
    assertEquals(Position(0, 1) overlap Position(0, 0), false)
    assertEquals(Position(1, 1) overlap Position(0, 0), false)
    assertEquals(Position(2, 0) overlap Position(0, 0), false)
    assertEquals(Position(0, 2) overlap Position(0, 0), false)
    assertEquals(Position(2, 2) overlap Position(0, 0), false)
  }

  test("head tail close") {
    assertEquals(Position(0, 0) close Position(0, 0), true)
    assertEquals(Position(1, 0) close Position(0, 0), true)
    assertEquals(Position(-1, 0) close Position(0, 0), true)
    assertEquals(Position(0, 1) close Position(0, 0), true)
    assertEquals(Position(0, -1) close Position(0, 0), true)
    assertEquals(Position(1, 1) close Position(0, 0), true)

    assertEquals(Position(2, 0) close Position(0, 0), false)
    assertEquals(Position(-2, 0) close Position(0, 0), false)
    assertEquals(Position(0, 2) close Position(0, 0), false)
    assertEquals(Position(0, -2) close Position(0, 0), false)
    assertEquals(Position(2, 2) close Position(0, 0), false)
  }

  test("rope movement overlap") {
    val up = Rope.start.up
    val down = Rope.start.down
    val left = Rope.start.left
    val right = Rope.start.right
    assertEquals(up, Rope(Position(0, 1), Position(0, 0)))
    assertEquals(down, Rope(Position(0, -1), Position(0, 0)))
    assertEquals(left, Rope(Position(-1, 0), Position(0, 0)))
    assertEquals(right, Rope(Position(1, 0), Position(0, 0)))
  }

  test("rope movement go back") {
    val up = Rope.start.up.down
    val down = Rope.start.down.up
    val left = Rope.start.left.right
    val right = Rope.start.right.left
    assertEquals(up, Rope(Position(0, 0), Position(0, 0)))
    assertEquals(down, Rope(Position(0, 0), Position(0, 0)))
    assertEquals(left, Rope(Position(0, 0), Position(0, 0)))
    assertEquals(right, Rope(Position(0, 0), Position(0, 0)))
  }

  test("rope movement close") {
    val upLeft = Rope.start.up.left
    val downRight = Rope.start.down.right
    val leftUp = Rope.start.left.up
    val rightDown = Rope.start.right.down
    assertEquals(upLeft, Rope(Position(-1, 1), Position(0, 0)))
    assertEquals(downRight, Rope(Position(1, -1), Position(0, 0)))
    assertEquals(leftUp, Rope(Position(-1, 1), Position(0, 0)))
    assertEquals(rightDown, Rope(Position(1, -1), Position(0, 0)))
  }

  test("rope movement not close") {
    val upup = Rope.start.up.up
    val downdown = Rope.start.down.down
    val leftleft = Rope.start.left.left
    val rightright = Rope.start.right.right
    assertEquals(upup, Rope(Position(0, 2), Position(0, 1)))
    assertEquals(downdown, Rope(Position(0, -2), Position(0, -1)))
    assertEquals(leftleft, Rope(Position(-2, 0), Position(-1, 0)))
    assertEquals(rightright, Rope(Position(2, 0), Position(1, 0)))
  }

  test("rope movement not close diagonal") {
    val upupLeft = Rope.start.up.up.left
    val downdownRight = Rope.start.down.down.right
    val leftleftUp = Rope.start.left.left.up
    val rightrightDown = Rope.start.right.right.down
    assertEquals(upupLeft, Rope(Position(-1, 2), Position(0, 1)))
    assertEquals(downdownRight, Rope(Position(1, -2), Position(0, -1)))
    assertEquals(leftleftUp, Rope(Position(-2, 1), Position(-1, 0)))
    assertEquals(rightrightDown, Rope(Position(2, -1), Position(1, 0)))
  }

  test("rope movement not close diagonal 2") {
    val leftUpup = Rope.start.left.up.up
    val rightDowndown = Rope.start.right.down.down
    val upLeftleft = Rope.start.up.left.left
    val downRightright = Rope.start.down.right.right
    assertEquals(leftUpup, Rope(Position(-1, 2), Position(-1, 1)))
    assertEquals(rightDowndown, Rope(Position(1, -2), Position(1, -1)))
    assertEquals(upLeftleft, Rope(Position(-2, 1), Position(-1, 1)))
    assertEquals(downRightright, Rope(Position(2, -1), Position(1, -1)))
  }

  test("right4 up4 left3 down1 right4b down1 left5 right2") {
    val right4 = move(Rope.start, List(Right, Right, Right, Right)).last
    val up4 = move(right4, List(Up, Up, Up, Up)).last
    val left3 = move(up4, List(Left, Left, Left)).last
    val down1 = move(left3, List(Down)).last
    val right4b = move(down1, List(Right, Right, Right, Right)).last
    val down1b = move(right4b, List(Down)).last
    val left5 = move(down1b, List(Left, Left, Left, Left, Left)).last
    val right2 = move(left5, List(Right, Right)).last
    assertEquals(right4, Rope(Position(4, 0), Position(3, 0)))
    assertEquals(up4, Rope(Position(4, 4), Position(4, 3)))
    assertEquals(left3, Rope(Position(1, 4), Position(2, 4)))
    assertEquals(down1, Rope(Position(1, 3), Position(2, 4)))
    assertEquals(right4b, Rope(Position(5, 3), Position(4, 3)))
    assertEquals(down1b, Rope(Position(5, 2), Position(4, 3)))
    assertEquals(left5, Rope(Position(0, 2), Position(1, 2)))
    assertEquals(right2, Rope(Position(2, 2), Position(1, 2)))
  }

  test("day 9 part 2") {
    assertEquals(Day9.part2(input2), 36)
  }

  test("right5 up8") {
    val right5 = moveLong(LongRope.start, List.fill(5)(Right)).last
    val up8 = moveLong(right5, List.fill(8)(Up)).last
    assertEquals(right5,
      LongRope(Position(5, 0), Position(4, 0), Position(3, 0), Position(2, 0), Position(1, 0),
               Position(0, 0), Position(0, 0), Position(0, 0), Position(0, 0), Position(0, 0)))
    assertEquals(up8,
      LongRope(Position(5, 8), Position(5, 7), Position(5, 6), Position(5, 5), Position(5, 4),
               Position(4, 4), Position(3, 3), Position(2, 2), Position(1, 1), Position(0, 0)))
  }