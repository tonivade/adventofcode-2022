package day7

class Day7Suite extends munit.FunSuite:

  val input = """$ cd /
                |$ ls
                |dir a
                |14848514 b.txt
                |8504156 c.dat
                |dir d
                |$ cd a
                |$ ls
                |dir e
                |29116 f
                |2557 g
                |62596 h.lst
                |$ cd e
                |$ ls
                |584 i
                |$ cd ..
                |$ cd ..
                |$ cd d
                |$ ls
                |4060174 j
                |8033020 d.log
                |5626152 d.ext
                |7214296 k""".stripMargin

  test("day 7 part 1") {
    assertEquals(Day7.part1(input), 95437)
  }

//  test("day 7 part 2") {
//    assertEquals(Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19)
//  }