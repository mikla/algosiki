package leetcode.strings

object _520_DetectCapital extends App {

  def detectCapitalUse(word: String): Boolean = {
    word.toLowerCase == word || word.toUpperCase == word || {
      word.head.toUpper == word.head.toUpper && word.tail.toLowerCase == word.tail
    }
  }

}
