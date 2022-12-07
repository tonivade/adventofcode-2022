package day7

import scala.io.Source

object Day7:

  sealed trait Node
  case class File(name: String, size: Int) extends Node
  case class Directory(name: String) extends Node
  case class ChangeDir(name: String) extends Node
  case object ListDir extends Node
  case object ExitDir extends Node

  def part1(input: String): Int = 
    val chdir = "\\$ cd ([/\\w]+)".r
    val dir = "dir (\\w+)".r
    val file = "(\\d+) ([\\.\\w]+)".r
    val result = input.split("\n")
      .map {
        case "$ ls" => ListDir
        case "$ cd .." => ExitDir
        case chdir(name) => ChangeDir(name)
        case dir(name) => Directory(name)
        case file(size, name) => File(name, size.toInt)
      }
      .foldLeft((List.empty[String], Map.empty[String, List[Node]])) {
        case ((path, state), ChangeDir(name)) => (path :+ name, state)
        case ((path, state), ListDir) => (path, state)
        case ((path, state), ExitDir) => (path.dropRight(1), state)
        case ((path, state), d: Directory) => (path, update(state, path.last, d))
        case ((path, state), f: File) => (path, update(state, path.last, f))
      }._2

    result.map {
      case (name, _) => calculate(name, result)
    }
    .filter(_ < 100000)
    .sum

  def update(state: Map[String, List[Node]], current: String, node: Node): Map[String, List[Node]] = 
    val content = state.getOrElse(current, List.empty)
    state + (current -> (content :+ node))

  def calculate(name: String, state: Map[String, List[Node]], current: Int = 0): Int =
    state.getOrElse(name, List.empty).foldLeft(current) {
      case (c, File(_, size)) => c + size
      case (c, Directory(n)) => calculate(n, state, c)
    }

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
