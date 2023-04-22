package common.tree

import leetcode.TreeNode

// Functional Data Structures
sealed trait FTree[+A]

case class FNode[A](left: FTree[A], value: A, right: FTree[A]) extends FTree[A]

case object Empty extends FTree[Nothing]

object FTree {

  /** Convert leetcode TreeNode to FTree
    */
  def fromTreeNode(treeNode: TreeNode): FTree[Int] = {
    if (treeNode == null) Empty
    else
      FNode(
        fromTreeNode(treeNode.left),
        treeNode.value,
        fromTreeNode(treeNode.right)
      )
  }

  def toTreeNode(tree: FTree[Int]): TreeNode = {
    tree match {
      case Empty => null
      case FNode(left, value, right) =>
        new TreeNode(value, toTreeNode(left), toTreeNode(right))
    }
  }

}
