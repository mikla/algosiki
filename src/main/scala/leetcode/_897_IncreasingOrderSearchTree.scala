package leetcode

import common.tree.{Empty, FNode, FTree}

object _897_IncreasingOrderSearchTree extends App {

  def increasingBST(root: TreeNode): TreeNode = {
    val froot = FTree.fromTreeNode(root)

    def traverse(tree: FTree[Int]): List[Int] = tree match {
      case FNode(value, left, right) =>
        traverse(left) ++ List(value) ++ traverse(right)
      case Empty => Nil
    }

    val asnFTree = traverse(froot).reverse match {
      case head :: next =>
        Some(next.foldLeft(FNode(head): FTree[Int]) { case (node, value) =>
          FNode(value, Empty, node)
        })
      case Nil => None
    }

    asnFTree.map(FTree.toTreeNode).orNull
  }

  val test = FNode(
    5,
    FNode(3, FNode(2, FNode(1)), FNode(4)),
    FNode(6, Empty, FNode(8, FNode(7), FNode(9)))
  )

}
