package leetcode

import common.tree.{Empty, FNode, FTree}

object _2236_RootEqualsSum extends App {

  def checkTree(root: TreeNode): Boolean = {
    val free = FTree.fromTreeNode(root)

    free match {
      case FNode(value, FNode(l, Empty, Empty), FNode(r, Empty, Empty)) =>
        value == l + r
      case Empty => false
    }
  }

}
