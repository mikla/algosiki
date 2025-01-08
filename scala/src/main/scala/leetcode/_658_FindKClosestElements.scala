package leetcode

object _658_FindKClosestElements extends App {

  object Solution {
    def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
      arr
        .sortBy(a => Math.abs(a - x))
        .take(k)
        .toList
        .sorted
    }
  }

  println(Solution.findClosestElements(Array(1,2,3,4,5), 4, 3))
  println(Solution.findClosestElements(Array(1,2,3,4,5), 4, -1))
  println(Solution.findClosestElements(Array(0, 0, 1, 2, 3, 3, 4, 7, 7, 8), 3, 5))

}
