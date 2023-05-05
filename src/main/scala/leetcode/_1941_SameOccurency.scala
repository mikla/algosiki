package leetcode

object _1941_SameOccurency extends App {

  def areOccurrencesEqual(s: String): Boolean = {
    s.groupBy(identity).view.mapValues(_.length).values.toSet.size == 1
  }

  println(areOccurrencesEqual(""))

}
