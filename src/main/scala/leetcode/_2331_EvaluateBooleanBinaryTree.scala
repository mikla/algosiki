package leetcode

import common.tree.{FNode, FTree}

object _2331_EvaluateBooleanBinaryTree extends App {

  def eval(tree: FTree[Int]): Boolean = tree match {
    case FNode(1, _, _) => true
    case FNode(0, _, _) => false
    case FNode(2, l, r) =>
      eval(l) || eval(r)
    case FNode(3, l, r) =>
      eval(l) && eval(r)
    case _ => false
  }

  def evaluateTree(root: TreeNode): Boolean = {
    val ftree = FTree.fromTreeNode(root)
    eval(ftree)
  }

  val test = FNode(2, FNode(1), FNode(3, FNode(0), FNode(1)))

  println(eval(test))

}
