package leetcode

object _112_PathSums extends App {

  object Solution {

    def isLeaf(treeNode: TreeNode): Boolean =
      treeNode.left == null && treeNode.right == null

    def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {

      def loop(
          node: TreeNode,
          currentSum: Int
      ): Boolean = {
        if (node != null) {
          val recur = loop(_, currentSum + node.value)
          if (currentSum + node.value == targetSum && isLeaf(node)) true
          else recur(node.left) || recur(node.right)
        } else false
      }

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
