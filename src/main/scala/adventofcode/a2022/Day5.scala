package adventofcode.a2022

import helpers.Input

object Day5 extends App {

  val init =
    Map(
      1 -> "SPHVFG",
      2 -> "MZDVBFJG",
      3 -> "NJLMG",
      4 -> "PWDVZGN",
      5 -> "BCRV",
      6 -> "ZLWPMSRV",
      7 -> "PHT",
      8 -> "VZHCNSRQ",
      9 -> "JQVPGLF"
    )

  val instructions =
    Input.readListString("src/main/scala/adventofcode/a2022/Day5.in")

  def sol(m: Map[Int, String]) =
    m.view
      .mapValues(_.headOption)
      .toList
      .sortBy(_._1)
      .flatMap(_._2)
      .mkString("")

  def solution(reverse: Boolean) = instructions
    .foldLeft(init) { case (acc, s"move $count from $from to $to") =>
      val updated = {
        val toTake = acc(from.toInt).take(count.toInt)
        if (reverse) toTake.reverse else toTake
      }.concat(acc(to.toInt))
      val removed = acc(from.toInt).drop(count.toInt)

      acc
        .updated(from.toInt, removed)
        .updated(to.toInt, updated)
    }

  val part1 = solution(reverse = true)
  val part2 = solution(reverse = false)

  println(sol(part1))
  println(sol(part2))

}
