package leetcode

object _49_GroupAnagrams extends App {

  object Solution {
    def groupAnagrams(strs: Array[String]): List[List[String]] = {
      strs.groupBy(_.sorted).values.toList.map(_.toList)
    }

  }

  println(Solution.groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))
  println(Solution.groupAnagrams(Array("cab","tin","pew","duh","may","ill","buy","bar","max","doc")))

}
