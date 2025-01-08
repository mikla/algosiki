package helpers

import scala.io.Source

object Input {

  def readListString(file: String): List[String] = {
    Source.fromFile(file).getLines().toList
  }

  def readListStringGrouped(file: String): List[List[String]] = {
    Source
      .fromFile(file)
      .getLines()
      .foldLeft((List.empty[List[String]], List.empty[String])) {
        case ((all, acc), elem) =>
          if (elem.isBlank) (all.appended(acc), List.empty)
          else (all, acc.appended(elem))
      }
      ._1
  }

  def readAsString(file: String): String = {
    Source.fromFile(file).getLines().mkString
  }

  def readListInt(file: String): List[Int] = readListString(file).map(_.toInt)

  def readCommaSeparatedInts(file: String) =
    readAsString(file).split(",").map(_.toInt).toList

}
