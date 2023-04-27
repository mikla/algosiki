package leetcode.linkedlist

import leetcode.ListNode

object _19_RemoveNthNodeFromEndOfList extends App {

  object Solution {
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      val l = scala.collection.mutable.ArrayBuffer(head)
      var h = head
      while (h.next != null) {
        h = h.next
        l.append(h)
      }

      val (left, right) = l.splitAt(l.size - n)

      left ++ right.tail
      if (left.nonEmpty) {
        left.last.next = right.tail.headOption.orNull
        left.head
      } else {
        right.tail.headOption.orNull
      }
    }
  }

  val testNode =
    new ListNode(
      1,
      new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5))))
    )

  val fromListNode1 = ListNode.fromList(List(1, 2, 3, 4, 5))
  val fromListNode2 = ListNode.fromList(List(1))
  val fromListNode3 = ListNode.fromList(List(1, 2))

  println(ListNode.toList(fromListNode1))

  println(ListNode.toList(Solution.removeNthFromEnd(testNode, 2)))
  println(ListNode.toList(Solution.removeNthFromEnd(fromListNode2, 1)))
  println(ListNode.toList(Solution.removeNthFromEnd(fromListNode3, 1)))

}
