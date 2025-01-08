package object leetcode {

  // Default leetcode structures
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  class Node(var _value: Int) {
    var value: Int = _value
    var children: List[Node] = List()
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object ListNode {

    def fromList(l: List[Int]): ListNode =
      l.init.foldRight(new ListNode(l.last)) { case (elem, acc) =>
        new ListNode(elem, acc)
      }

    def toList(node: ListNode): List[Int] = {
      if (node != null) {
        val l = scala.collection.mutable.ArrayBuffer(node.x)
        var h = node
        while (h.next != null) {
          h = h.next
          l.append(h.x)
        }
        l.toList
      } else Nil
    }
  }

}
