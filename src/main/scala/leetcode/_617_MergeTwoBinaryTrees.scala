package leetcode

import common.tree.{Empty, FNode, FTree}

object _617_MergeTwoBinaryTrees extends App {

  def mergeTrees(root1: TreeNode, root2: TreeNode): TreeNode = {
    val froot1 = FTree.fromTreeNode(root1)
    val froot2 = FTree.fromTreeNode(root2)

    def merge(fr1: FTree[Int], fr2: FTree[Int]): FTree[Int] = {
      (fr1, fr2) match {
        case (FNode(v1, fr1left, fr1right), FNode(v2, fr2left, fr2right)) =>
          FNode(v1 + v2, merge(fr1left, fr2left), merge(fr1right, fr2right))
        case (ln @ FNode(_, _, _), Empty) => ln
        case (Empty, rn @ FNode(_, _, _)) => rn
        case _                            => Empty
      }
    }

    FTree.toTreeNode(merge(froot1, froot2))
  }

}
