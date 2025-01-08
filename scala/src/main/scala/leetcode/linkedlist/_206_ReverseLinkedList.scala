package leetcode.linkedlist

import leetcode.ListNode

object _206_ReverseLinkedList extends App {

  def reverseList(head: ListNode): ListNode = {
    def loop(l: ListNode, acc: List[Int]): List[Int] =
      if (l == null) acc
      else {
        loop(l.next, l.x :: acc)
      }

    val elements = loop(head, Nil)

    if (elements.isEmpty) null
    else {
      elements.init.foldRight(new ListNode(elements.last)) { case (e, acc) =>
        new ListNode(e, acc)
      }
    }
  }

}
