package helpers

import scala.io.Source

object Input {

  def readListString(file: String): List[String] = {
    Source.fromFile(file).getLines().toList
  }

  def readAsString(file: String): String = {
    Source.fromFile(file).getLines().mkString
  }

  def readListInt(file: String): List[Int] = readListString(file).map(_.toInt)

  def readCommaSeparatedInts(file: String) = readAsString(file).split(",").map(_.toInt).toList

}
