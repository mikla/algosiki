package leetcode

import common.tree.{Empty, FNode, FTree}

object _938_RangeSumOfBST extends App {

  def rangeSumBST(root: TreeNode, low: Int, high: Int): Int = {
    val froot = FTree.fromTreeNode(root)

    def loop(tree: FTree[Int]): Int = tree match {
      case FNode(value, left, right) =>
        val inclValue = if (low <= value && value <= high) value else 0
        inclValue + loop(left) + loop(right)
      case Empty => 0
    }

    loop(froot)
  }

  val tree =
    FNode(10, FNode(5, FNode(3), FNode(7)), FNode(15, Empty, FNode(18)))

  println(rangeSumBST(FTree.toTreeNode(tree), 7, 15))

}
