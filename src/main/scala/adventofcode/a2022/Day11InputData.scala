package adventofcode.a2022

import adventofcode.a2022.Day11.Monkey

object Day11InputData {

  lazy val Test0: List[Monkey] =
    List(
      Monkey(0, List(79, 98), _ * 19, 23, 2, 3),
      Monkey(1, List(54, 65, 75, 74), _ + 6, 19, 2, 0),
      Monkey(2, List(79, 60, 97), x => x * x, 13, 1, 3),
      Monkey(3, List(74), _ + 3, 17, 0, 1)
    )


  lazy val Test1: List[Monkey] =
    List(
      Monkey(0, List(54, 82, 90, 88, 86, 54), _ * 7, 11, 2, 6),
      Monkey(1, List(91, 65), _ * 13, 5, 7, 4),
      Monkey(2, List(62, 54, 57, 92, 83, 63, 63), _ + 1, 7, 1, 7),
      Monkey(3, List(67, 72, 68), x => x * x, 2, 0, 6),
      Monkey(4, List(68, 89, 90, 86, 84, 57, 72, 84), _ + 7, 17, 3, 5),
      Monkey(5, List(79, 83, 64, 58), _ + 6, 13, 3, 0),
      Monkey(6, List(96, 72, 89, 70, 88), _ + 4, 3, 1, 2),
      Monkey(7, List(79), _ + 8, 19, 4, 5),
    )

}
