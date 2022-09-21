package leetcode

object _985_SumOfEvenNumbersAfterQueries extends App {

  object Solution {

    def sumEvenAfterQueries(
        nums: Array[Int],
        queries: Array[Array[Int]]
    ): Array[Int] = {

      val sum = nums.filter(_ % 2 == 0).sum

      queries
        .collect { case Array(add, index) =>
          (add, index)
        }
        .foldLeft((List.empty[Int], sum)) { case ((ans, sum), (add, index)) =>
          val sum_ : Int = if (nums(index) % 2 == 0) sum - nums(index) else sum
          nums(index) += add
          val sum_1 = if (nums(index) % 2 == 0) sum_ - nums(index) else sum_
          (sum_1 :: ans, sum_1)
        }
        ._1
        .reverse
        .toArray

    }
  }

}
