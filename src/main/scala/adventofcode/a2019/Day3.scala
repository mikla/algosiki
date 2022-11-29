package adventofcode.a2019

import helpers.Input

object Day3 extends App {

  case class Coord(x: Long, y: Long) {
    def right(rightX: Long): Coord = copy(x = x + rightX, y)
    def up(upY: Long): Coord = copy(x, y = y + upY)
    def left(leftX: Long): Coord = copy(x = x - leftX, y)
    def down(downY: Long): Coord = copy(x, y = y - downY)
  }

  case class Line(start: Coord, end: Coord) {
    def toCoords: List[Coord] = {
      if (start.x == end.x) {
        (start.y to end.y by step(start.y, end.y)).map(Coord(start.x, _)).toList
      } else if (start.y == end.y) {
        (start.x to end.x by step(start.x, end.x)).map(Coord(_, start.y)).toList
      } else Nil
    }
  }

  def step(x: Long, y: Long) =
    if (x <= y) 1 else -1

  def distance(c1: Coord, c2: Coord): Long =
    Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y)

  def react(init: Coord, instruction: String): Coord =
    instruction match {
      case s"U$y" => init.up(y.toInt)
      case s"D$y" => init.down(y.toInt)
      case s"L$x" => init.left(x.toInt)
      case s"R$x" => init.right(x.toInt)
    }

  def buildVectors(instructions: List[String]): List[Line] =
    instructions
      .scanLeft(Coord(0, 0))(react)
      .sliding(2)
      .map { case start :: end :: Nil =>
        Line(start, end)
      }
      .toList

  val input =
    Input.readListString("src/main/scala/adventofcode/a2019/Day3.in")

  val InitCoord = Coord(0, 0)

  val wire1 = input.head
  val wire2 = input(1)

  val wire1Vectors = buildVectors(wire1.split(",").toList)
  val wire2Vectors = buildVectors(wire2.split(",").toList)

  val wires1points = InitCoord :: wire1Vectors.flatMap(_.toCoords.tail)
  val wires2points = InitCoord :: wire2Vectors.flatMap(_.toCoords.tail)

  val intersections = wires1points
    .filter(wires2points.contains)
    .filterNot(_ == InitCoord)

  val part1 = intersections
    .map(c => (c, distance(InitCoord, c)))
    .sortBy(_._2)
    .headOption
    .map(_._2)

  println(part1)

  val part2 = intersections
    .map { coord =>
      val byFirstWirePath = wires1points.takeWhile(_ != coord).length
      val bySecondWirePath = wires2points.takeWhile(_ != coord).length

      byFirstWirePath + bySecondWirePath
    }
    .minOption
    .getOrElse(0)

  println(part2)

}
