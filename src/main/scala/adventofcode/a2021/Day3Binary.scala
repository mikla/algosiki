package adventofcode.a2021

import helpers.Input

object Day3Binary extends App {

  val input = Input
    .readListString(
      "src/main/scala/adventofcode/a2021/Day3Binary.in"
    )

  val distribution = input
    .foldLeft(Map.empty[Int, (Int, Int)]) {
      case (acc, line) =>
        line.zipWithIndex.foldLeft(acc) {
          case (a, (char, index)) =>
            val (zeros, ones) = a.getOrElse(index, (0, 0))
            if (char == '0') a.updated(index, (zeros + 1, ones))
            else a.updated(index, (zeros, ones + 1))
        }
    }

  val mostCommon =
    distribution
      .map {
        case (key, (zeros, ones)) =>
          key -> (if (zeros > ones) 0 else 1)
      }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString

  val leastCommon =
    distribution
      .map {
        case (key, (zeros, ones)) =>
          key -> (if (zeros > ones) 1 else 0)
      }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString

  val gamma = Integer.parseInt(mostCommon, 2)
  val eps = Integer.parseInt(leastCommon, 2)

  println(gamma)
  println(eps)

  println(gamma * eps)

}
