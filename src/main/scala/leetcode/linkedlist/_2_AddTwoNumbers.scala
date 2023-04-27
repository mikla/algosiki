package leetcode.linkedlist

import leetcode.ListNode

object _2_AddTwoNumbers extends App {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    def reverseListNode(l: ListNode): ListNode = {

      def loop(l: ListNode, acc: List[Int]): List[Int] =
        if (l == null) acc
        else {
          loop(l.next, l.x :: acc)
        }

      val elements = loop(l, Nil)

      elements.init.foldRight(new ListNode(elements.last)) { case (e, acc) =>
        new ListNode(e, acc)
      }
    }

    def loop(
        l1node: ListNode,
        l2node: ListNode,
        carry: Int,
        acc: ListNode
    ): ListNode = {
      (Option(l1node), Option(l2node)) match {
        case (None, None) =>
          if (carry == 1) new ListNode(1, acc) else acc
        case (None, Some(node)) =>
          loop(
            null,
            node.next,
            (node.x + carry) / 10,
            new ListNode((node.x + carry) % 10, acc)
          )
        case (Some(node), None) =>
          loop(
            null,
            node.next,
            (node.x + carry) / 10,
            new ListNode((node.x + carry) % 10, acc)
          )
        case (Some(node1), Some(node2)) =>
          loop(
            node1.next,
            node2.next,
            (node1.x + node2.x + carry) / 10,
            new ListNode((node1.x + node2.x + carry) % 10, acc)
          )
      }
    }

    reverseListNode(loop(l1, l2, 0, null))
  }

//  println(ListNode.toList(reverseListNode(addTwoNumbers(n1, n2))))
//
//  val n1 = new ListNode(2, new ListNode(4, new ListNode(3)))
//  val n2 = new ListNode(5, new ListNode(6, new ListNode(4)))
//
//  val ans = addTwoNumbers(n1, n2)

}
