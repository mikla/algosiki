package leetcode

import common.tree.{Empty, BNode, BTree}

object _617_MergeTwoBinaryTrees extends App {

  def mergeTrees(root1: TreeNode, root2: TreeNode): TreeNode = {
    val froot1 = BTree.fromTreeNode(root1)
    val froot2 = BTree.fromTreeNode(root2)

    def merge(fr1: BTree[Int], fr2: BTree[Int]): BTree[Int] = {
      (fr1, fr2) match {
        case (BNode(v1, fr1left, fr1right), BNode(v2, fr2left, fr2right)) =>
          BNode(v1 + v2, merge(fr1left, fr2left), merge(fr1right, fr2right))
        case (ln @ BNode(_, _, _), Empty) => ln
        case (Empty, rn @ BNode(_, _, _)) => rn
        case _                            => Empty
      }
    }

    BTree.toTreeNode(merge(froot1, froot2))
  }

}
