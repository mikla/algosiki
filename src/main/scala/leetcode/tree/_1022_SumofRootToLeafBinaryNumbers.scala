package leetcode.tree

import common.tree.{BNode, BTree, Empty}
import leetcode.TreeNode

object _1022_SumofRootToLeafBinaryNumbers extends App {

  def sumRootToLeaf(root: TreeNode): Int = {

    def findAllBinaries(
        btree: BTree[Int],
        acc: List[Int],
        all: List[List[Int]]
    ): List[List[Int]] =
      btree match {
        case BNode(value, Empty, Empty) =>
          (acc :+ value) :: all

        case BNode(value, left, right) =>
          findAllBinaries(left, acc :+ value, all) ++
            findAllBinaries(right, acc :+ value, all)
        case Empty => Nil
      }

    println(
      findAllBinaries(
        BTree.fromTreeNode(root),
        Nil,
        Nil
      ).map(_.mkString(""))
    )

    findAllBinaries(
      BTree.fromTreeNode(root),
      Nil,
      Nil
    ).map(_.mkString("")).map(Integer.parseInt(_, 2)).sum
  }

  println(sumRootToLeaf(new TreeNode(1, new TreeNode(1))))

}
