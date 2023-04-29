package leetcode.strings

// https://leetcode.com/problems/reverse-string/
object _344_ReverseString extends App {

  def reverseString(s: Array[Char]): Unit = {
    var i = 0

    while (i < s.length / 2) {
      val t = s(i)
      s(i) = s(s.length - 1 - i)
      s(s.length - 1 - i) = t

      i += 1
    }
  }

}
