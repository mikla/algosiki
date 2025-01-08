package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _226_InvertBinaryTree extends App {

  def invertTree(root: TreeNode): TreeNode = {
    val broot = BTree.fromTreeNode(root)

    def invert(bTree: BTree[Int]): BTree[Int] = bTree match {
      case BNode(value, left, right) =>
        BNode(value, invert(right), invert(left))
      case Empty => Empty
    }

    BTree.toTreeNode(invert(broot))
  }

}
