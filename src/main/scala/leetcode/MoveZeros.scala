package leetcode

// https://leetcode.com/problems/move-zeroes/
object MoveZeros extends App {

  def moveZeroes(nums: Array[Int]): Unit = {
    var i = 0

    val ans = new Array[Int](nums.length)
    var z = 0

    while (i < nums.length) {
      if (nums(i) != 0) {
        ans(z) = nums(i)
        z += 1
      }

      i += 1
    }

    ans.copyToArray(nums)
  }

  moveZeroes(Array(0, 1, 2, 3, 4, 0, 0, 12, 34))
  moveZeroes(Array(0))
}
