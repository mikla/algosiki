package leetcode

object _1456_MaximumNumberVowels extends App {

  def isVowel(s: Char): Boolean = s match {
    case 'a' | 'e' | 'i' | 'o' | 'u' => true
    case _                           => false
  }

  def maxVowels(s: String, k: Int): Int = {
    var initCount = s.take(k).count(isVowel)
    var max = initCount
    var i = k

    while (i < s.length) {
      if (isVowel(s(i))) initCount += 1
      if (isVowel(s(i - k))) initCount -= 1
      max = Math.max(max, initCount)
      i += 1
    }

    max
  }

}
