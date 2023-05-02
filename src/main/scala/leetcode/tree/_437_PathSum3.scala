package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _437_PathSum3 extends App {

  def pathSum(root: TreeNode, targetSum: Int): Int = {
    val froot = BTree.fromTreeNode(root)

    def loop(treeNode: BTree[Int], sum: Long): Int = {
      treeNode match {
        case BNode(value, left, right) =>
          val interMedSum = sum + value
          val counter = if (interMedSum == targetSum) 1 else 0
          counter + loop(left, interMedSum) + loop(right, interMedSum)
        case Empty => 0
      }
    }

    froot match {
      case n @ BNode(_, left, right) =>
        pathSum(BTree.toTreeNode(left), targetSum) + pathSum(
          BTree.toTreeNode(right),
          targetSum
        ) + loop(n, 0)
      case Empty => 0
    }
  }

  val testTree = BNode(
    10,
    BNode(5, BNode(3, BNode(3), BNode(2)), BNode(2, Empty, BNode(1))),
    BNode(-3, Empty, BNode(11))
  )

  println(pathSum(BTree.toTreeNode(testTree), 8))

}
