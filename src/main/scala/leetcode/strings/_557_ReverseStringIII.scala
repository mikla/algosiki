package leetcode.strings

// https://leetcode.com/problems/reverse-words-in-a-string-iii/
object _557_ReverseStringIII extends App {

  def reverseWords(s: String): String = {
    s.split(" ").map(_.reverse).mkString(" ")
  }

  println(reverseWords("Let's take LeetCode contest"))
  println(reverseWords("God Ding"))

}
