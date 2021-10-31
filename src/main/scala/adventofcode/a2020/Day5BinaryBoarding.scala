package adventofcode.a2020

import helpers.Input

object Day5BinaryBoarding extends App {

  val lines = Input.readListString("src/main/scala/a2020/Day5BinaryBoarding.in")

  val rangeAdjust: (String, Char) => String = (r, char) =>
    r + (char match {
      case 'F' | 'L' => "0"
      case 'B' | 'R' => "1"
    })

  def toDec(repr: String) = Integer.parseInt(repr.foldLeft("")(rangeAdjust), 2)

  def seatId(row: Int, col: Int) = row * 8 + col

  val ids = lines.map(str => seatId(toDec(str.substring(0, 7)), toDec(str.substring(7))))

  val ans1 = ids.max

  println(ans1)

  println {
    ids.sorted.sliding(3)
      .collectFirst {
        case a :: b :: c :: Nil if a + 1 == b && b + 1 != c => b + 1
      }
  }

}
