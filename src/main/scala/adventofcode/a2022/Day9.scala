package adventofcode.a2022

import adventofcode.common.Point
import adventofcode.common.Point.PointI
import helpers.Input

object Day9 extends App {

  case class State(visited: List[PointI], head: PointI, tail: PointI) {

    def moveRight(x: Int): State = {
      ???
    }

    def moveUp(y: Int): State = {
      ???
    }

    def moveLeft(x: Int): State = {
      ???
    }

    def moveDown(y: Int): State = {
      ???
    }

  }

  object State {
    val Init = State(List(Point(0, 0)), Point(0, 0), Point(0, 0))
  }

  val inputList: List[String] =
    Input.readListString("src/main/scala/adventofcode/a2022/Day9.in")

  inputList.foldLeft(State.Init) { case (state, command) =>
    command match {

      case s"R $x" => ???

      case s"U $y" => ???

      case s"L $x" => ???

      case s"U $y" => ???

    }
  }

}
