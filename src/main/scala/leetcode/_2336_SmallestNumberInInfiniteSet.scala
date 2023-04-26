package leetcode

import java.util.PriorityQueue
import scala.collection.mutable

object _2336_SmallestNumberInInfiniteSet extends App {

  class SmallestInfiniteSet() {
    val isPresent = mutable.HashSet[Integer]()

    implicit val reverseOrdering = implicitly[Ordering[Integer]].reverse
    val addedIntegers = mutable.PriorityQueue[Integer]()
    var currentInteger: Int = 1

    def popSmallest(): Int = {
      if (addedIntegers.nonEmpty) {
        val ans = addedIntegers.dequeue()
        isPresent.remove(ans)
        ans
      } else {
        val ans = currentInteger
        currentInteger = currentInteger + 1
        ans
      }
    }

    new PriorityQueue().poll()

    def addBack(num: Int): Unit = {
      if (!(currentInteger <= num || isPresent.contains(num))) {
        addedIntegers.enqueue(num)
        isPresent.add(num)
      }
    }

  }

  val set = new SmallestInfiniteSet()
  println(set.popSmallest())
  set.addBack(1)
  println(set.popSmallest())
  println(set.popSmallest())
  println(set.popSmallest())
  set.addBack(2)
  set.addBack(3)

  println(set.popSmallest())
  println(set.popSmallest())

}
