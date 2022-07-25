package adventofcode.a2021

import helpers.Input

object Day5Hydrothermal extends App {

  sealed trait Line
  case class Horizontal(y: Int, from: Int, to: Int) extends Line
  case class Vertical(x: Int, from: Int, to: Int) extends Line
  case class Diagonal(from: (Int, Int), to: (Int, Int)) extends Line

  val input = Input
    .readListString("src/main/scala/adventofcode/a2021/Day5Hydrothermal.in")
    .map {
      case s"$x1,$y1 -> $x2,$y2" => (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt)
    }
    .map {
      case ((x1, y1), (x2, y2)) if x1 == x2 =>
        Vertical(x1, List(y1, y2).min, List(y1, y2).max)
      case ((x1, y1), (x2, y2)) if y1 == y2 =>
        Horizontal(y1, List(x1, x2).min, List(x1, x2).max)
      case (p1 @ (x1, y1), p2 @ (x2, y2))
          if Math.abs(x1 - x2) == Math.abs(y1 - y2) =>
        Diagonal(p1, p2)
    }

  val bitmap = Array.fill[Int](1000, 1000)(0)

  def incPosition(x: Int, y: Int): Unit = {
    val curr = bitmap(x)(y)
    bitmap(x)(y) = curr + 1
  }

  def drawLine(line: Line) =
    line match {
      case Horizontal(y, from, to) =>
        (from to to).foreach { x => incPosition(x, y) }
      case Vertical(x, from, to) =>
        (from to to).foreach { y => incPosition(x, y) }
      case Diagonal((x1, y1), (x2, y2)) =>
        if (x1 < x2 && y1 > y2)
          (x1 to x2).foldLeft(y1) {
            case (y, x) =>
              incPosition(x, y)
              y - 1
          }
        else if (x1 < x2 && y1 < y2)
          (x1 to x2).foldLeft(y1) {
            case (y, x) =>
              incPosition(x, y)
              y + 1
          }
        else if (x1 > x2 && y1 > y2)
          (x2 to x1).foldLeft(y2) {
            case (y, x) =>
              incPosition(x, y)
              y + 1
          }
        else if (x1 > x2 && y1 < y2)
          (x2 to x1).foldLeft(y2) {
            case (y, x) =>
              incPosition(x, y)
              y - 1
          }
    }

  input.foreach(drawLine)

  println(bitmap.map(_.count(_ > 1)).sum)

}
