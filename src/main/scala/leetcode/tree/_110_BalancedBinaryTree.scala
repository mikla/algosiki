package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _110_BalancedBinaryTree extends App {

  def isBalanced(root: TreeNode): Boolean = {
    def depth(r: BTree[Int]): Int = r match {
      case BNode(_, left, right) =>
        1 + Math.max(depth(left), depth(right))
      case Empty => 0
    }

    def isBalancedF(br: BTree[Int]): Boolean = br match {
      case BNode(_, left, right) =>
        val ldepth = depth(left)
        val rdepth = depth(right)

        Math.abs(ldepth - rdepth) <= 1 && isBalancedF(left) && isBalancedF(right)
      case Empty => true
    }

    isBalancedF(BTree.fromTreeNode(root))
  }

}
