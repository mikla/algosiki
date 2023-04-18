package leetcode

object _1768_MergeStrings extends App {

  def mergeAlternately(word1: String, word2: String): String = {
    val minStrLen = Math.min(word1.length, word2.length)
    val tail = word1.drop(minStrLen) + word2.drop(minStrLen)
    (word1.take(minStrLen) zip word2.take(minStrLen)).map { case (c1, c2) =>
      s"$c1$c2"
    }.mkString + tail
  }

}
