package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _897_IncreasingOrderSearchTree extends App {

  def increasingBST(root: TreeNode): TreeNode = {
    val froot = BTree.fromTreeNode(root)

    def traverse(tree: BTree[Int]): List[Int] = tree match {
      case BNode(value, left, right) =>
        traverse(left) ++ List(value) ++ traverse(right)
      case Empty => Nil
    }

    val asnFTree = traverse(froot).reverse match {
      case head :: next =>
        Some(next.foldLeft(BNode(head): BTree[Int]) { case (node, value) =>
          BNode(value, Empty, node)
        })
      case Nil => None
    }

    asnFTree.map(BTree.toTreeNode).orNull
  }

  val test = BNode(
    5,
    BNode(3, BNode(2, BNode(1)), BNode(4)),
    BNode(6, Empty, BNode(8, BNode(7), BNode(9)))
  )

}
