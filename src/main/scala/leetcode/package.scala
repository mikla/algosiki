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

}
