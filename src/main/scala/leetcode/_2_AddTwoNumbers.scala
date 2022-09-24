package leetcode

object _2_AddTwoNumbers extends App {

  object Solution {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      var l1n = l1
      var l2n = l2

      while (l1n.next != null & l2n.next != null) {
        val digit = (l1n.x + l2n.x) % 10
        val carry = (l1n.x + l2n.x) / 10



      }
      ???
    }
  }



}
