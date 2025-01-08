package adventofcode.a2020

import helpers.Input

import scala.annotation.tailrec

// https://adventofcode.com/2020/day/3
object Day3TobogganTrajectory extends App {

  val lines =
    Input.readListString("src/main/scala/a2020/Day3TobogganTrajectory.in")

  val arr = lines.map(_.toCharArray).toArray

  @tailrec
  def loop(
      initX: Int,
      initY: Int,
      trees: Int,
      nextX: Int => Int,
      nextY: Int => Int
  ): Long = {
    if (initY > arr.length - 1) trees
    else {
      val xx = initX % arr(0).length
      if (arr(initY)(xx) == '#')
        loop(nextX(initX), nextY(initY), trees + 1, nextX, nextY)
      else
        loop(nextX(initX), nextY(initY), trees, nextX, nextY)
    }
  }

  val ans: Long =
    loop(1, 1, 0, _ + 1, _ + 1) *
      loop(3, 1, 0, _ + 3, _ + 1) *
      loop(5, 1, 0, _ + 5, _ + 1) *
      loop(7, 1, 0, _ + 7, _ + 1) *
      loop(1, 2, 0, _ + 1, _ + 2)

  println(ans)

}
