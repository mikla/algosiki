package leetcode

object _918_Maximum_Sum_CircularSubarray extends App {

  def maxSubarraySumCircular(nums: Array[Int]): Int = {

    val maxFn: (Int, (Int, List[Int])) => (Int, List[Int]) = {
      case (elem, (max, arr)) =>
        if (elem >= max) (elem, elem :: arr)
        else (max, max :: arr)
    }

    val originalMax = nums.init
      .scanRight(nums.last)(_ + _)
      .init
      .foldRight((Integer.MIN_VALUE, List.empty[Int]))(maxFn)

    val extendedMax = nums.init
      .scanLeft(0)(_ + _)
      .tail

    val localMax = extendedMax.zipWithIndex.map { case (elem, index) =>
      originalMax._2(index + 1) + elem
    }

    localMax.toList.max
  }

  println(maxSubarraySumCircular(Array(1, -2, 3, -2)))
//  println(maxSubarraySumCircular(Array(5, -3, 5)))
//  println(maxSubarraySumCircular(Array(-3, -2, -3)))

}
