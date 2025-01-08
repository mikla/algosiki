package adventofcode.a2022

import helpers.Input

object Day10 extends App {

  val input = Input.readListString("src/main/scala/adventofcode/a2022/Day10.in")

  val state = input
    .foldLeft(List(1)) { case (history, operation) =>
      operation match {
        case s"noop" => history.head :: history
        case s"addx $x" =>
          history.head + (x.toInt) :: history.head :: history
      }
    }
    .reverse

  val registers = List(20, 60, 100, 140, 180, 220)

  println(registers.map(x => state(x - 1) * x).sum)

  val screen = List.fill(240)(" ") // 0 .. 239

  state.zipWithIndex
    .foldLeft(screen) { case (screen, (spriteStart, cycle)) =>
      val offset = (cycle / 40) * 40 - 1

      if (cycle <= spriteStart + offset + 2 && cycle >= spriteStart + offset)
        screen.updated(cycle, "█")
      else screen
    }
    .mkString("")
    .grouped(40)
    .foreach(println)

}
