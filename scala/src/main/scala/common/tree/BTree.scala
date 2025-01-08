package common.tree

import leetcode.TreeNode

// Functional Data Structures
sealed trait BTree[+A]

case class BNode[A](value: A, left: BTree[A] = Empty, right: BTree[A] = Empty)
    extends BTree[A]

case object Empty extends BTree[Nothing]

object BTree {

  /** Convert leetcode TreeNode to FTree
    */
  def fromTreeNode(treeNode: TreeNode): BTree[Int] = {
    if (treeNode == null) Empty
    else
      BNode(
        treeNode.value,
        fromTreeNode(treeNode.left),
        fromTreeNode(treeNode.right)
      )
  }

  def toTreeNode(tree: BTree[Int]): TreeNode = {
    tree match {
      case Empty => null
      case BNode(value, left, right) =>
        new TreeNode(value, toTreeNode(left), toTreeNode(right))
    }
  }

}
