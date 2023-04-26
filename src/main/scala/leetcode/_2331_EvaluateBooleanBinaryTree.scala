package leetcode

import common.tree.{BNode, BTree}

object _2331_EvaluateBooleanBinaryTree extends App {

  def eval(tree: BTree[Int]): Boolean = tree match {
    case BNode(1, _, _) => true
    case BNode(0, _, _) => false
    case BNode(2, l, r) =>
      eval(l) || eval(r)
    case BNode(3, l, r) =>
      eval(l) && eval(r)
    case _ => false
  }

  def evaluateTree(root: TreeNode): Boolean = {
    val ftree = BTree.fromTreeNode(root)
    eval(ftree)
  }

  val test = BNode(2, BNode(1), BNode(3, BNode(0), BNode(1)))

  println(eval(test))

}
