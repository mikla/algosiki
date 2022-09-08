package leetcode

// https://leetcode.com/problems/binary-tree-pruning/
object _814_BinaryTreePruning extends App {

  object Solution {
    def pruneTree(root: TreeNode): TreeNode = {

      def allZero(tree: TreeNode): Boolean = {
        val currNode = tree.value == 0

        val left = if (tree.left != null) allZero(tree.left) else true
        val right = if (tree.right != null) allZero(tree.right) else true

        val toPrune = currNode && left && right

        if (left) tree.left = null
        if (right) tree.right = null

        toPrune
      }

      if (allZero(root)) null else root
    }
  }

  val testTree1 = new TreeNode(
    1,
    new TreeNode(0, new TreeNode(0), new TreeNode(0)),
    new TreeNode(1, new TreeNode(0), new TreeNode(1))
  )

  Solution.pruneTree(testTree1)

}
