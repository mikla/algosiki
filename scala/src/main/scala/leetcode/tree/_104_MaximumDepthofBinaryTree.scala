package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _104_MaximumDepthofBinaryTree extends App {

  def maxDepth(root: TreeNode): Int = {

    def depth(btree: BTree[Int]): Int = btree match {
      case BNode(_, left, right) =>
        Math.max(1 + depth(left), 1 + depth(right))
      case Empty => 0
    }

    depth(BTree.fromTreeNode(root))
  }

}
