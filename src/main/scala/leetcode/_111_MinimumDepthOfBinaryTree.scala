package leetcode

import common.tree._

object _111_MinimumDepthOfBinaryTree extends App {

  def minDepth(root: TreeNode): Int = {

    def depth(btree: BTree[Int], d: Int): List[Int] = btree match {
      case BNode(value, l: BNode[Int], r: BNode[Int]) =>
        depth(l, d + 1) ++ depth(r, d + 1)
      case BNode(_, l: BNode[Int], Empty) => depth(l, d + 1)
      case BNode(_, Empty, r: BNode[Int]) => depth(r, d + 1)
      case BNode(_, Empty, Empty)         => List(d)
      case _                              => Nil
    }

    if (root == null) 0 else depth(BTree.fromTreeNode(root), 1).min
  }

}
