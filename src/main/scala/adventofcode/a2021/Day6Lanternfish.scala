package adventofcode.a2021

import helpers.Input

object Day6Lanternfish extends App {

  val input = Input
    .readCommaSeparatedInts(
      "src/main/scala/adventofcode/a2021/Day6Lanternfish.in"
    )

  def dec: List[Int] => List[Int] = _.map(_ - 1)

  def create(l: List[Int]): List[Int] =
    l.flatMap { e =>
      if (e == -1) List(6, 8) else List(e)
    }

  val after = (1 to 256).foldLeft(input) { case (acc, i) =>
    println(s"day ${i}")
    create(dec(acc))
  }

  println(after.size)

}
