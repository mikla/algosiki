package adventofcode.a2020

import helpers.Input

// https://adventofcode.com/2020/day/2
object Day2PasswordPhilosophy extends App {

  val lines = Input.readListString("src/main/scala/a2020/Day2PasswordPhilosophy.in")

  println {
    lines.count {
      case s"$a-$b $l: $pass" =>
        val lINPass = pass.count(_.toString == l)
        lINPass >= a.toInt && lINPass <= b.toInt
    }
  }

  println {
    lines.count {
      case s"$a-$b $l: $pass" =>
        Seq(pass.charAt(a.toInt - 1), pass.charAt(b.toInt - 1)).count(_.toString == l) == 1
    }
  }

}
