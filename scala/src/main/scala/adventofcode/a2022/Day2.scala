package adventofcode.a2022

import adventofcode.a2022.Day2.GameResult.{Draw, Loose, Win}
import adventofcode.a2022.Day2.RPS.{Paper, Rock, Scissor}
import helpers.Input

object Day2 extends App {

  trait GameResult {
    val points: Int
    val repr: String
  }

  object GameResult {

    val All = List(Win, Loose, Draw)

    case object Win extends GameResult {
      override val points: Int = 6
      override val repr: String = "Z"
    }

    case object Loose extends GameResult {
      override val points: Int = 0
      override val repr: String = "X"
    }

    case object Draw extends GameResult {
      override val points: Int = 3
      override val repr: String = "Y"
    }

    def fromString(s: String): Option[GameResult] = All.find(_.repr.contains(s))

  }

  trait RPS {
    val repr: List[String]
    val weight: Int
  }

  object RPS {
    val All: Seq[RPS] = List(Rock, Paper, Scissor)
    def fromString(s: String): Option[RPS] = All.find(_.repr.contains(s))

    case object Rock extends RPS {
      override val repr: List[String] = List("A", "X")
      override val weight: Int = 1
    }

    case object Paper extends RPS {
      override val repr: List[String] = List("B", "Y")
      override val weight: Int = 2
    }

    case object Scissor extends RPS {
      override val repr: List[String] = List("C", "Z")
      override val weight: Int = 3
    }
  }

  def game(opponent: RPS, me: RPS): GameResult = (opponent, me) match {
    case (Rock, Scissor)    => Loose
    case (Scissor, Paper)   => Loose
    case (Paper, Rock)      => Loose
    case (Rock, Rock)       => Draw
    case (Scissor, Scissor) => Draw
    case (Paper, Paper)     => Draw
    case _                  => Win
  }

  def game2(opponent: RPS, gameResult: GameResult): RPS =
    (opponent, gameResult) match {
      case (o, Draw)        => o
      case (Paper, Loose)   => Rock
      case (Rock, Loose)    => Scissor
      case (Scissor, Loose) => Paper
      case (Paper, Win)     => Scissor
      case (Rock, Win)      => Paper
      case (Scissor, Win)   => Rock
    }

  val in = Input.readListString("src/main/scala/adventofcode/a2022/Day2.in")

  val part1 = in
    .map(_.split(" ").flatMap(RPS.fromString) match {
      case Array(e1, e2) => (e1, e2)
    })
    .map { case (opponentS, meS) =>
      game(opponentS, meS).points + meS.weight
    }
    .sum

  val part2 = in
    .flatMap(_.split(" ").toList match {
      case e1 :: e2 :: Nil =>
        for {
          o <- RPS.fromString(e1)
          r <- GameResult.fromString(e2)
        } yield (o, r)
    })
    .map { case (opponent, result) =>
      game2(opponent, result).weight + result.points
    }
    .sum

  println(part1)
  println(part2)

}
