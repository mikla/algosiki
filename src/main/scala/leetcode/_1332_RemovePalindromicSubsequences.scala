package leetcode

/** https://leetcode.com/problems/remove-palindromic-subsequences/description/
  */
object _1332_RemovePalindromicSubsequences extends App {

  def removePalindromeSub(s: String): Int = {
    if (s.isEmpty) 0
    else if (s == s.reverse) 1
    else 2
  }

//  val isPalindrome = (s: String) => s == s.reverse
//
//  def removePalindromicSteps(s: String): Int = {
//    def loop(currS: String, currPal: String, acc: Int): Int =
//      if (currS.isEmpty) acc
//      else {
//        if (isPalindrome(currPal)) loop(currS, currPal, acc)
//      }
//  }
//
}
