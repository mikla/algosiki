package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _700_SearchInBST extends App {

  def searchBST(root: TreeNode, `val`: Int): TreeNode = {
    val froot = BTree.fromTreeNode(root)

    def search(fTree: BTree[Int]): Option[BTree[Int]] = fTree match {
      case t @ BNode(value, left, right) =>
        if (value == `val`) Some(t)
        else search(left).orElse(search(right))
      case Empty => None
    }
    search(froot).map(BTree.toTreeNode).orNull
  }

}
