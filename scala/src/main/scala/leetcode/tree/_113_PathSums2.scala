package leetcode.tree

import leetcode.TreeNode

object _113_PathSums2 extends App {

  object Solution {

    def isLeaf(treeNode: TreeNode): Boolean =
      treeNode.left == null && treeNode.right == null

    def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] = {

      def loop(
          node: TreeNode,
          currentSum: Int,
          path: List[Int]
      ): List[List[Int]] = {
        if (node != null) {
          val recur = loop(_, currentSum + node.value, path :+ node.value)
          if (currentSum + node.value == targetSum && isLeaf(node)) {
            List(path :+ node.value)
          } else recur(node.left) ++ recur(node.right)
        } else Nil
      }

      loop(root, 0, Nil)

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


  println(Solution.pathSum(testTree1, 22))
  println(Solution.pathSum(testTree2, -5))
}
