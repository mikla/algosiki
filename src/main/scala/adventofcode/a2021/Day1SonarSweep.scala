package adventofcode.a2021

import helpers.Input

object Day1SonarSweep extends App {

  val input = Input
    .readListInt("src/main/scala/adventofcode/a2021/Day1SonarSweepSample.in")

  val (increasedTimes, _) = input.tail.foldLeft((0, input.head))(increaseSum)

  println(increasedTimes)

  val tripples = input.sliding(3, 1).toList.map(_.sum)
  val (increasedTimes1, _) =
    tripples.tail.foldLeft((0, tripples.head))(increaseSum)

  private def increaseSum(acc: (Int, Int), elem: Int) = {
    if (acc._2 < elem) (acc._1 + 1, elem)
    else (acc._1, elem)
  }

  println(increasedTimes1)

}
