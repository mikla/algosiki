package leetcode

object _112_PathSums extends App {

  object Solution {

    def isLeaf(treeNode: TreeNode): Boolean =
      treeNode.left == null && treeNode.right == null

    def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {

      def loop(
          node: TreeNode,
          currentSum: Int
      ): Boolean =
        (node != null) && (
          (currentSum + node.value == targetSum && isLeaf(node)) ||
            loop(node.left, currentSum + node.value) ||
            loop(node.right, currentSum + node.value)
        )

      loop(root, 0)
    }

  }

  val testTree1 =
    new TreeNode(
      5,
      new TreeNode(4, new TreeNode(11, new TreeNode(7), new TreeNode(2))),
      new TreeNode(
        8,
        new TreeNode(13),
        new TreeNode(4, new TreeNode(5), new TreeNode(1))
      )
    )

  val testTree2 =
    new TreeNode(
      -2,
      null,
      new TreeNode(-3)
    )

  println(Solution.hasPathSum(testTree1, 22))
  println(Solution.hasPathSum(testTree2, -5))
}
