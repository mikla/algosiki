package leetcode

//https://leetcode.com/problems/count-good-nodes-in-binary-tree/

object CountGoodNodesInBinaryTree extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def goodNodes(root: TreeNode): Int = {
      isGood(root.value, root)
    }

    def isGood(maxOnTheWay: Int, node: TreeNode): Int = {
      val currentNode = if (maxOnTheWay <= node.value) 1 else 0
      val localMax = Math.max(maxOnTheWay, node.value)

      val leftGoodNodes = if (node.left != null) isGood(localMax, node.left) else 0
      val rightGoodNodes = if (node.right != null) isGood(localMax, node.right) else 0

      currentNode + leftGoodNodes + rightGoodNodes
    }
  }

  trait Tree
  case class TreeNode2(value: Int = 0, left: Tree = Empty, right: Tree = Empty) extends Tree
  case object Empty extends Tree

  object Solution2 {
    def goodNodes(root: Tree): Int = {
      root match {
        case TreeNode2(value, l, r) =>
          1 + isGood(value, l) + isGood(value, r)
      }
    }

    def isGood(maxOnTheWay: Int, node: Tree): Int = {
      node match {
        case TreeNode2(value, l, r) =>
          val currentNode = if (maxOnTheWay <= value) 1 else 0
          val localMax = Math.max(maxOnTheWay, value)
          currentNode + isGood(localMax, l) + isGood(localMax, r)
        case Empty => 0

      }
    }
  }

  val testTree1 = TreeNode2(3,
    TreeNode2(1,
      TreeNode2(3),
    ),
    TreeNode2(4, TreeNode2(1), TreeNode2(5))
  )

  val testTree2 = TreeNode2(3,
    TreeNode2(3, TreeNode2(4), TreeNode2(2))
  )

  val testTree3 = TreeNode2(1)

  println(Solution2.goodNodes(testTree1))
  println(Solution2.goodNodes(testTree2))
  println(Solution2.goodNodes(testTree3))

}
