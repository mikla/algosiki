package leetcode.tree

import leetcode.TreeNode

// https://leetcode.com/problems/binary-tree-preorder-traversal/
object _144_BinaryTreePreorderTraversal {

  def preorderTraversal(root: TreeNode): List[Int] = {

    def loop(tree: TreeNode): List[Int] = {
      if (tree == null) Nil
      else
        List(tree.value) ++ loop(tree.left) ++ loop(tree.right)
    }

    loop(root)

  }

}
