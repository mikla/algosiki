package leetcode

// https://leetcode.com/problems/construct-string-from-binary-tree/

object _606_ConstructStringFromBinaryTree extends App {

  object Solution {
    def tree2str(root: TreeNode): String = {

      def loop(tree: TreeNode, acc: String): String = {
        val curr = tree.value.toString

        val left = if (tree.left != null) "(" + loop(tree.left, acc) else ")"
        val right = if (tree.right != null) "(" + loop(tree.right, acc) else ")"

        curr + left + right
      }

      loop(root, "").replace("()", "")

    }
  }

  val testTree1 = new TreeNode(1, new TreeNode(2, new TreeNode(4)), new TreeNode(3))

  println(Solution.tree2str(testTree1))

}
