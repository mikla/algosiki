package adventofcode.a2021

import helpers.Input

object Day7Treachery extends App {

  val input = Input
    .readCommaSeparatedInts(
      "src/main/scala/adventofcode/a2021/Day7Treachery.in"
    )

  val distribution = input.groupBy(identity).values.toList.sortBy(_.length)

  def move(l: List[Int])(moveTo: Int): Int =
    l.foldLeft(0) { case (acc, e) =>
      if (e == moveTo) acc
      else acc + Math.abs(moveTo - e)
    }

  println(distribution.map(r => move(input)(r.head)).min)

  // part two

  def calCost(distance: Int): Int = (1 + distance) * distance / 2

  def move2(l: List[Int])(moveTo: Int): Int =
    l.foldLeft(0) { case (acc, e) =>
      if (e == moveTo) acc
      else acc + calCost(Math.abs(moveTo - e))
    }

  println((input.min to input.max).map(move2(input)).min)

  println(calCost(11))

}
