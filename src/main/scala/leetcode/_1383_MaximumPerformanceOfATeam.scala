package leetcode

import scala.collection.mutable



object _1383_MaximumPerformanceOfATeam extends App {

  object Solution {

    case class Engineer(speed: Int, efficiency: Int)
    case class Acc(result: Long, totalSpeed: Long) {
      def addSpeed(sp: Long): Acc = this.copy(totalSpeed = totalSpeed + sp)
      def addResult(eff: Long): Acc =
        this.copy(result = Math.max(result, eff * totalSpeed))
    }

    def maxPerformance(
        n: Int,
        speed: Array[Int],
        efficiency: Array[Int],
        k: Int
    ): Int = {

      val minHeap =
        mutable.PriorityQueue.empty(implicitly[Ordering[Long]].reverse)

      val result = speed
        .zip(efficiency)
        .map(Engineer.tupled)
        .sortBy(-_.efficiency)
        .foldLeft(Acc(0, 0)) { case (acc, engineer) =>
          val acc_ = if (minHeap.length == k) {
            acc.addSpeed(-minHeap.dequeue())
          } else acc

          minHeap.enqueue(engineer.speed)

          acc_
            .addSpeed(engineer.speed)
            .addResult(engineer.efficiency)
        }
        .result

      (result % 1000000007).toInt
    }
  }

  val res = Solution.maxPerformance(
    6,
    Array(2, 10, 3, 1, 5, 8),
    Array(5, 4, 3, 9, 7, 2),
    2
  )

  println(res)

}
