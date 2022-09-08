package leetcode

// https://leetcode.com/problems/binary-tree-inorder-traversal/
object _94_BinaryTreeInorderTraversal {

  def inorderTraversal(root: TreeNode): List[Int] = {

    def loop(tree: TreeNode): List[Int] = {
      if (tree == null) Nil
      else
        loop(tree.left) ++ List(tree.value) ++ loop(tree.right)
    }

    loop(root)

  }

}
