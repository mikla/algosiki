package common.tree

import leetcode.TreeNode

// Functional Data Structures
sealed trait FTree[+A]

case class FNode[A](value: A, left: FTree[A] = Empty, right: FTree[A] = Empty)
    extends FTree[A]

case object Empty extends FTree[Nothing]

object FTree {

  /** Convert leetcode TreeNode to FTree
    */
  def fromTreeNode(treeNode: TreeNode): FTree[Int] = {
    if (treeNode == null) Empty
    else
      FNode(
        treeNode.value,
        fromTreeNode(treeNode.left),
        fromTreeNode(treeNode.right)
      )
  }

  def toTreeNode(tree: FTree[Int]): TreeNode = {
    tree match {
      case Empty => null
      case FNode(value, left, right) =>
        new TreeNode(value, toTreeNode(left), toTreeNode(right))
    }
  }

}
