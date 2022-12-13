package adventofcode.a2022

import helpers.Input

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day12Part1 extends App {

  case class Point(x: Int, y: Int) {
    def up = copy(x = x - 1)
    def down = copy(x = x + 1)
    def left = copy(y = y - 1)
    def right = copy(y = y + 1)

    def neighbours: List[Point] =
      List(up, down, left, right)
  }

  val abc = ('a' to 'z').zipWithIndex
  def indexOf(char: Char): Int =
    abc.find(_._1 == char).map(_._2).getOrElse(-1)

  val input = Input
    .readListString("src/main/scala/adventofcode/a2022/Day12.in")
    .map(_.toList)

  val indexedInput = input.map(_.zipWithIndex).zipWithIndex

  def findChar(s: Char): Point = {
    val row = indexedInput.find(_._1.exists(_._1 == s)).map(_._2).get
    val col = indexedInput(row)._1.find(_._1 == s).map(_._2).get
    Point(row, col)
  }

  val spos = findChar('S')
  val epos = findChar('E')

  if (input(spos.x)(spos.y) != 'S') sys.error("Wrong start position")
  if (input(epos.x)(epos.y) != 'E') sys.error("Wrong start position")

  val MaxX = input.size - 1
  val MaxY = input.head.size - 1

  val isField: Point => Boolean = p =>
    p.x >= 0 && p.x <= MaxX && p.y >= 0 && p.y <= MaxY

  val distance: (Point, Point) => Int = (init, to) => {
    val startChar =
      if (input(init.x)(init.y) == 'S') 'a' else input(init.x)(init.y)
    val endChar = if (input(to.x)(to.y) == 'E') 'z' else input(to.x)(to.y)
    indexOf(endChar) - indexOf(startChar)
  }

  implicit val ord: Ordering[(Point, Int)] =
    (x: (Point, Int), y: (Point, Int)) => y._2 - x._2

  def search(start: Point, end: Point): Int = {
    val visitedPoints = ArrayBuffer.empty[Point]
    val path = ArrayBuffer.empty[Point]
    val queue = mutable.PriorityQueue[(Point, Int)]((start, 0))

    var reachedTheEnd = false
    var shortest = 0

    while (queue.nonEmpty && !reachedTheEnd) {
      val (cnode, d) = queue.dequeue()

      if (cnode == end) {
        reachedTheEnd = true
        shortest = d
      } else {
        if (!visitedPoints.contains(cnode)) {
          visitedPoints.addOne(cnode)

          path.addOne(cnode)

          queue.addAll(
            cnode.neighbours
              .filter(isField)
              .filter(distance(cnode, _) <= 1)
              .map(n => (n, d + 1))
          )
        }
      }
    }

    shortest
  }

  println(search(spos, epos))

}
