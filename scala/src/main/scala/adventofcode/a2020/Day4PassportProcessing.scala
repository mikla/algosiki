package adventofcode.a2020

import helpers.Input

object Day4PassportProcessing extends App {

  val lines =
    Input.readListString("src/main/scala/a2020/Day4PassportProcessing.in") :+ ""

  val Required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val Optional = Set("cid")

  val rules: Map[String, String => Boolean] = Map(
    ("byr", (s: String) => s.toIntOption.exists(y => y >= 1920 && y <= 2002)),
    (
      "iyr",
      (s: String) => s.toIntOption.exists(y => y >= 2010 && s.toInt <= 2020)
    ),
    (
      "eyr",
      (s: String) => s.toIntOption.exists(y => y >= 2020 && s.toInt <= 2030)
    ),
    (
      "hgt",
      {
        case s"${n}cm" => n.toInt >= 150 && n.toInt <= 193
        case s"${n}in" => n.toInt >= 59 && n.toInt <= 76
        case _         => false
      }
    ),
    ("hcl", "^#([a-f0-9]{6})$".r.matches),
    ("ecl", List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains),
    ("pid", s => s.forall(_.isDigit) && s.length == 9)
  )

  def validate(values: Map[String, String]) = values.forall { case (k, v) =>
    rules(k).apply(v)
  } && values.keys.toSet == Required

  val ans = lines
    .foldLeft(("", List.empty[String])) { case ((acc, normalized), newLine) =>
      if (newLine.trim.isBlank) ("", acc :: normalized)
      else (acc + " " + newLine, normalized)
    }
    ._2
    .map { passportInfo =>
      passportInfo.trim
        .split(" ")
        .map(l => l.substring(0, 3) -> l.substring(4).trim)
        .toMap -- Optional
    }
    .count(validate)

  println(ans)
}
