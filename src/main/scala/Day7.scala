package day7

import scala.io.Source
import scala.collection.mutable.HashMap

object Day7:

  sealed trait Node
  sealed trait Content extends Node
  case class File(name: String, size: Int) extends Content
  case class Directory(name: String) extends Content
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
      .foldLeft((List.empty[String], Map.empty[String, List[Content]])) {
        case ((path, state), ChangeDir(name)) => (path :+ name, state)
        case ((path, state), ListDir) => (path, state)
        case ((path, state), ExitDir) => (path.dropRight(1), state)
        case ((path, state), file: File) => (path, update(state, path, file))
        case ((path, state), directory: Directory) => (path, update(state, path, directory))
      }._2

    calculate(result)
      .values
      .filter(_ < 100000)
      .sum

  def update(state: Map[String, List[Content]], path: List[String], file: File): Map[String, List[Content]] = 
    val name = path.mkString(",")
    val content = state.getOrElse(name, List.empty)
    state + (name -> (content :+ file))

  def update(state: Map[String, List[Content]], path: List[String], directory: Directory): Map[String, List[Content]] = 
    val name = path.mkString(",")
    val content = state.getOrElse(name, List.empty)
    state + (name -> (content :+ Directory(name + "," + directory.name)))

  def calculate(all: Map[String, List[Content]]): Map[String, Int] =
    val result = HashMap.empty[String, Int]
    val status = HashMap.from(all)
    while (!status.isEmpty) {
      val directories = status.filter {
        case (name, content) => content.forall {
          case Directory(name) => result.contains(name)
          case _ => true
        }
      }.map(_._1)

      directories.foreach { dir =>
        val dirSize = status.remove(dir).get.foldLeft(0) {
          case (current, File(name, size)) => current + size
          case (current, Directory(name)) => current + result.get(name).get
        }
        result.put(dir, dirSize)
      }
    }
    Map.from(result)

@main def main: Unit =
  val input = Source.fromFile("input/day7.txt").getLines().mkString("\n")
  println(Day7.part1(input))
