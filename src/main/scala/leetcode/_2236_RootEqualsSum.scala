package leetcode

import common.tree.{Empty, BNode, BTree}

object _2236_RootEqualsSum extends App {

  def checkTree(root: TreeNode): Boolean = {
    val free = BTree.fromTreeNode(root)

    free match {
      case BNode(value, BNode(l, Empty, Empty), BNode(r, Empty, Empty)) =>
        value == l + r
      case Empty => false
    }
  }

}
