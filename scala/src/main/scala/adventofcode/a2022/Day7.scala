package adventofcode.a2022

import helpers.Input

import scala.annotation.tailrec

object Day7 extends App {

  trait FileTree

  object FileTree {
    case class Folder(path: Path, parent: Option[Path]) extends FileTree
    case class File(path: Path, size: Long, parent: Path) extends FileTree
  }

  import FileTree._

  case class Path(segments: List[String] = List()) {

    def cd(folder: String): Path = folder match {
      case ".."   => Path(segments.init)
      case folder => Path(segments :+ folder)
    }

  }

  case class State(currentPath: Path, flat: List[FileTree])

  object State {
    def empty: State = State(Path(Nil), Nil)
  }

  @tailrec
  def parseFlat(commands: List[String], state: State): List[FileTree] = {
    if (commands.isEmpty) state.flat
    else {
      commands.head match {
        case s"cd $folder" =>
          parseFlat(
            commands.tail,
            state.copy(currentPath = state.currentPath.cd(folder))
          )

        case "ls" =>
          parseFlat(commands.tail, state)

        case s"dir $folder" =>
          val newState = state.copy(
            flat = Folder(
              path = state.currentPath.cd(folder),
              parent = Some(state.currentPath)
            ) :: state.flat
          )

          parseFlat(commands.tail, newState)

        case s"$size $name" =>
          val newState = state.copy(
            flat = File(
              path = state.currentPath.cd(name),
              size = size.toLong,
              parent = state.currentPath
            ) :: state.flat
          )

          parseFlat(commands.tail, newState)

      }
    }
  }

  def calculateSize(flat: List[FileTree]): List[(Folder, Long)] = {

    def loopLoop(folder: Folder, size: Long): Long =
      flat
        .filter {
          case Folder(_, Some(parent)) => parent == folder.path
          case File(_, _, parent)      => parent == folder.path
          case _                       => false
        }
        .map {
          case f: Folder        => loopLoop(f, size)
          case File(_, size, _) => size
        }
        .sum

    flat.collect { case f: Folder =>
      (f, loopLoop(f, 0))
    }
  }

  val input = Input.readListString("src/main/scala/adventofcode/a2022/Day7.in")

  val normInput = input.map(_.replace("$ ", "")).drop(1)

  val RootFolder = Folder(Path(Nil), None)

  val flatStructure = parseFlat(normInput, State.empty)

  val dirsInfo = calculateSize(RootFolder :: flatStructure)

  val AllFoldersSize = dirsInfo.find(_._1 == RootFolder).get._2

  val part1 = dirsInfo.filter(_._2 <= 100000).map(_._2).sum

  val part2 = dirsInfo
    .map { folder =>
      (folder, 70000000 - AllFoldersSize + folder._2)
    }
    .filter(_._2 >= 30000000)
    .map(_._1._2)
    .min

  println(part1)
  println(part2)

}
