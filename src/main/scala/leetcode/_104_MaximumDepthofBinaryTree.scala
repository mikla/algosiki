package leetcode

import common.tree.{BNode, BTree, Empty}

object _104_MaximumDepthofBinaryTree extends App {

  def maxDepth(root: TreeNode): Int = {

    def depth(btree: BTree[Int]): Int = btree match {
      case BNode(value, left, right) =>
        Math.max(1 + depth(left), 1 + depth(right))
      case Empty => 0
    }

    depth(BTree.fromTreeNode(root))
  }

}
