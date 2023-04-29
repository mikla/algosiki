package leetcode.strings

object _541_ReversString2 extends App {

  object Solution {
    def reverseStr(s: String, k: Int): String = {
      s.sliding(k, k)
        .toList
        .zipWithIndex
        .map { case (str, index) =>
          if (index % 2 == 0) str.reverse
          else str
        }
        .mkString("")
    }
  }

  println(Solution.reverseStr("abcdefg", 2))
  println(Solution.reverseStr("abcd", 2))

}
