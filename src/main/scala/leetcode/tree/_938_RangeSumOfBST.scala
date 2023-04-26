package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _938_RangeSumOfBST extends App {

  def rangeSumBST(root: TreeNode, low: Int, high: Int): Int = {
    val froot = BTree.fromTreeNode(root)

    def loop(tree: BTree[Int]): Int = tree match {
      case BNode(value, left, right) =>
        val inclValue = if (low <= value && value <= high) value else 0
        inclValue + loop(left) + loop(right)
      case Empty => 0
    }

    loop(froot)
  }

  val tree =
    BNode(10, BNode(5, BNode(3), BNode(7)), BNode(15, Empty, BNode(18)))

  println(rangeSumBST(BTree.toTreeNode(tree), 7, 15))

}
