package leetcode.array

object _718_MaximumLengthOfRepeatedSubarray extends App {

  object Solution {
    def findLength(nums1: Array[Int], nums2: Array[Int]): Int = {

      val arr = Array.fill[Int](nums1.length + 1, nums2.length + 1)(0)

      val nums1WithIndex = nums1.zipWithIndex.map { case (e, i) => (e, i + 1) }
      val nums2WithIndex = nums2.zipWithIndex.map { case (e, i) => (e, i + 1) }

      for {
        (num1, i) <- nums1WithIndex
        (num2, j) <- nums2WithIndex
      } yield
        if (num1 == num2)
          arr(i)(j) = arr(i - 1)(j - 1) + 1

      arr.map(_.max).max
    }
  }

  println(Solution.findLength(Array(1, 2, 3, 2, 1), Array(3, 2, 1, 4, 7)))

}
