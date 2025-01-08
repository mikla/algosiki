package adventofcode.a2022

import helpers.Input

object Day1 extends App {

  val input =
    Input.readListStringGrouped("src/main/scala/adventofcode/a2022/Day1.in")

  val elfCals = input.map { elefCalories =>
    elefCalories.map(_.toLong).sum
  }

  val part1 = elfCals.max

  println(part1)

  val part2 = elfCals.sorted.reverse.take(3).sum
  println(part2)

}
