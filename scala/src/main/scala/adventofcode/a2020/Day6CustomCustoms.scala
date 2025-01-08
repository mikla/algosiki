package adventofcode.a2020

import helpers.Input

object Day6CustomCustoms extends App {

  val lines =
    Input.readListString("src/main/scala/a2020/Day6CustomCustoms.in") :+ ""

  val ans1 = lines
    .foldLeft((List.empty[String], "")) { case ((acc, group), line) =>
      if (line == "") (group :: acc, "")
      else (acc, group + line)
    }
    ._1
    .map(_.toSet.size)
    .sum
  println(ans1)

}
