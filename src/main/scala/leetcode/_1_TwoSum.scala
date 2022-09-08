package leetcode

// https://leetcode.com/problems/two-sum/
object _1_TwoSum extends App {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    val numsWithIndexes = nums.zipWithIndex.sortBy(_._1)

    def searchIndex(left: Int, right: Int): List[Int] = {
      if (left < right) {
        val leftElem = numsWithIndexes(left)
        val rightElem = numsWithIndexes(right)

        val sum = leftElem._1 + rightElem._1
        if (sum == target) List(leftElem._2, rightElem._2)
        else if (sum > target) searchIndex(left, right - 1)
        else searchIndex(left + 1, right)

      } else Nil
    }

    searchIndex(0, numsWithIndexes.length - 1).toArray
  }

  println(_1_TwoSum.twoSum(Array(2, 7, 11, 15), 9).toList)
  println(_1_TwoSum.twoSum(Array(3, 2, 4), 6).toList)
  println(_1_TwoSum.twoSum(Array(3, 3), 6).toList)

}
