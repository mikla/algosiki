package adventofcode.a2021

import helpers.Input

object Day2Dive extends App {

  val input = Input
    .readListString("src/main/scala/adventofcode/a2021/Day2Dive.in")

  case class Position(depth: Int, horizontal: Int, aim: Int) {
    def up(by: Int) = this.copy(aim = aim - by)
    def down(by: Int) = this.copy(aim = aim + by)
    def forward(by: Int) =
      this.copy(
        horizontal = horizontal + by,
        depth = depth + (aim * by)
      )
    def mult = depth * horizontal
  }

  object Position {
    def Initial = Position(0, 0, 0)
  }

  sealed trait Instruction {
    def perform: Position => Position
  }

  object Instruction {

    def fromString(str: String): Instruction =
      str match {
        case s"forward ${by}" => Forward(by.toInt)
        case s"down ${by}"    => Down(by.toInt)
        case s"up ${by}"      => Up(by.toInt)
      }

    case class Forward(by: Int) extends Instruction {
      override def perform: Position => Position = _.forward(by)
    }
    case class Up(by: Int) extends Instruction {
      override def perform: Position => Position = _.up(by)
    }
    case class Down(by: Int) extends Instruction {
      override def perform: Position => Position = _.down(by)
    }

  }

  val star1 = input
    .map(Instruction.fromString)
    .foldLeft(Position.Initial) { case (position, instruction) =>
      instruction.perform(position)
    }
    .mult

  println(star1)

}
