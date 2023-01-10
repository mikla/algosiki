package leetcode

object _100_SameTree extends App {

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {

    def loop(tree: TreeNode): List[Option[Int]] = {
      if (tree == null) List(None)
      else
        List(Some(tree.value)) ++ loop(tree.left) ++ loop(tree.right)
    }

    loop(p) == loop(q)

  }

}
