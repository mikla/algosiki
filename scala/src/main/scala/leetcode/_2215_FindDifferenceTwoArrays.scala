package leetcode

object _2215_FindDifferenceTwoArrays extends App {

  def findDifference(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    val n1d = nums1.distinct.toList
    val n2d = nums2.distinct.toList

    List(
      n1d.diff(n2d),
      n2d.distinct.diff(n1d)
    )
  }

  println(findDifference(Array(1, 2, 3), Array(2, 4, 6)))
  println(findDifference(Array(1, 2, 3, 3), Array(1, 1, 2, 2)))
  println((Array(1, 1, 2, 2).distinct diff Array(1, 2, 3, 3).distinct).toList)

}
