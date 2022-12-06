package adventofcode.a2022

import helpers.Input

object Day6 extends App {

  val input = Input.readAsString("src/main/scala/adventofcode/a2022/Day6.in")

  def findMarker(slide: Int): Option[Int] =
    input
      .sliding(slide)
      .zipWithIndex
      .find { case (s, _) =>
        s.distinct == s
      }
      .map(_._2 + slide)

  val part1 = findMarker(4)
  val part2 = findMarker(14)

  println(part1)
  println(part2)

}
