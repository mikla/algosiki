package adventofcode.a2022

import helpers.Input
import spire.math.Interval

object Day4 extends App {

  val in = Input.readListString("src/main/scala/adventofcode/a2022/Day4.in")

  def interval(lower: Long, upper: Long): Interval[Long] =
    Interval(lower, upper)

  val intervalPairs = in.map(_.split(",").toList match {
    case first :: second :: Nil =>
      (first, second) match {
        case (s"$start1-$end1", s"$start2-$end2") =>
          (
            interval(start1.toLong, end1.toLong),
            interval(start2.toLong, end2.toLong)
          )
      }
  })

  val part1 = intervalPairs.count { case (i1, i2) =>
    ((i1 ∩ i2) == i1) || ((i2 ∩ i1) == i2)
  }

  val part2 = intervalPairs.count { case (i1, i2) =>
    !i1.overlap(i2).isDisjoint
  }

  println(part1)
  println(part2)

}
