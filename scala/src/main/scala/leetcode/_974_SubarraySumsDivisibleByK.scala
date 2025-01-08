package leetcode

object _974_SubarraySumsDivisibleByK extends App {

  def subarraysDivByK(nums: Array[Int], k: Int): Int = {
    val map = Map(0 -> 1)
    nums
      .foldLeft((map, 0, 0)) { case ((m, count, sum), elem) =>
        val sum1 = (sum + elem) % k
        val sum2 = if (sum1 < 0) sum1 + k else sum1
        val curCount = m.getOrElse(sum2, 0)
        val count1 = count + curCount

        (m.updated(sum2, curCount + 1), count1, sum2)
      }
      ._2
  }

}
