package leetcode

// 967 https://leetcode.com/problems/numbers-with-same-consecutive-differences/
object _967_NumbersWithSameConsecutiveDifferences extends App {

  object Solution {
    def numsSameConsecDiff(n: Int, k: Int): Array[Int] = {

      def search(
          digits: List[Int],
          digitsLeft: Int,
          acc: List[Int]
      ): List[Int] = {
        if (digitsLeft == 0) acc :+ digits.mkString("").toInt
        else {
          val lastDigit = digits.last
          val leftBranch =
            if (lastDigit + k <= 9)
              search(digits :+ (lastDigit + k), digitsLeft - 1, acc)
            else Nil
          val rightBranch =
            if (lastDigit - k >= 0)
              search(digits :+ (lastDigit - k), digitsLeft - 1, acc)
            else Nil

          leftBranch ++ rightBranch
        }
      }

      val result = (1 to 9)
        .map(startDigit => search(List(startDigit), n - 1, List.empty))
        .toList
        .flatten
        .distinct

      result.toArray
    }

  }

  println(Solution.numsSameConsecDiff(2, 0).toList)

}
