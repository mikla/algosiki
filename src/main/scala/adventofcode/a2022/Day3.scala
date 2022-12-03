package adventofcode.a2022

import helpers.Input

object Day3 extends App {

  val symbols = (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.map {
    case (c, index) => (c, index + 1)
  }

  def symbolPriority(s: Char): Option[Int] =
    symbols.find(_._1 == s).map(_._2)

  val input = Input.readListString("src/main/scala/adventofcode/a2022/Day3.in")

  val part1 = input.flatMap { str => 
    val ruksakSize = str.length()
    val (firstCompatiment, secondCompatiment) = (str.take(ruksakSize / 2), str.drop(ruksakSize / 2))
    val failed: String = (firstCompatiment intersect secondCompatiment).distinct
    symbolPriority(failed.head)
  }.sum

  println(part1)

  val part2 = input.grouped(3).flatMap {
    case first :: second :: third :: Nil =>
        val intesect1 = first intersect second
        val intersect2 = second intersect third
        val intersect3 = first intersect third
        val f = ((intesect1 intersect intersect2).intersect(intersect3)).distinct.head

        symbolPriority(f.toChar)
  }.sum

  println(part2)
  

}
