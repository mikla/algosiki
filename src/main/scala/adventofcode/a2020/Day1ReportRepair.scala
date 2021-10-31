package adventofcode.a2020

import helpers.Input

object Day1ReportRepair extends App {

  val input = Input.readListInt("src/main/scala/a2020/Day1Part2").distinct

  val ans = input.combinations(3).collect {
    case x :: y :: z :: Nil if x + y + z == 2020 => x * y * z
  }

  println(ans.toList.head)

}
