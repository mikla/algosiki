package leetcode

object _1457_PseudoPalindromicPathsInBinaryTree extends App {

  object Solution {

    // https://leetcode.com/problems/pseudo-palindromic-paths-in-a-binary-tree/discuss/2573860/Scala-Concise-functional-bitmask-solution
    def pseudoPalindromicPathsBit(root: TreeNode): Int = {
      def dfs(node: TreeNode, bitmask: Int): Int = {
        val newBitmask = 1 << node.value ^ bitmask

        if (node.left == null && node.right == null)
          if ((1 to 9).count(shift => (newBitmask >> shift & 1) > 0) <= 1) 1
          else 0
        else
          Seq(Option(node.left), Option(node.right)).flatten
            .map(dfs(_, newBitmask))
            .sum
      }

      dfs(root, bitmask = 0)
    }

    def pseudoPalindromicPaths(root: TreeNode): Int = {

      def loopMemoryLimit(treeNode: TreeNode): List[List[String]] = {
        if (isLeaf(treeNode))
          List(List(treeNode.value.toString))
        else {
          val left =
            if (treeNode.left != null) loopMemoryLimit(treeNode.left) else Nil
          val right =
            if (treeNode.right != null) loopMemoryLimit(treeNode.right) else Nil

          (left ++ right).map(_.prepended(treeNode.value.toString))
        }

      }

      def loop(treeNode: TreeNode, acc: List[Int]): Int = {
        if (isLeaf(treeNode))
          if (isPseudoPalindromic2(treeNode.value :: acc)) 1 else 0
        else {
          val l =
            if (treeNode.left != null)
              loop(treeNode.left, treeNode.value :: acc)
            else 0
          val r =
            if (treeNode.right != null)
              loop(
                treeNode.right,
                treeNode.value :: acc
              )
            else 0

          l + r
        }
      }

      loop(root, List.empty)
    }

    def isPseudoPalindromic2(paths: List[Int]): Boolean =
      isPseudoPalindromic(paths.map(_.toString))

    def isLeaf(treeNode: TreeNode): Boolean =
      treeNode.left == null && treeNode.right == null

    def isPseudoPalindromic(paths: List[String]): Boolean =
      paths
        .groupBy(identity)
        .toList
        .map(_._2.size)
        .count(_ % 2 == 1) <= 1

  }

  val testNode1 = new TreeNode(
    2,
    new TreeNode(3, new TreeNode(3), new TreeNode(1)),
    new TreeNode(1, null, new TreeNode(1))
  )

  val testNode2 = new TreeNode(
    2,
    new TreeNode(1, new TreeNode(1), new TreeNode(3, null, new TreeNode(1))),
    new TreeNode(1)
  )

  val testNode3 = new TreeNode(9)

  println(Solution.pseudoPalindromicPaths(testNode1))
  println(Solution.pseudoPalindromicPaths(testNode2))
  println(Solution.pseudoPalindromicPaths(testNode3))

}
