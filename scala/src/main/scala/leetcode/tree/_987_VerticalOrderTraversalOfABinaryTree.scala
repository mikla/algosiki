package leetcode.tree

import leetcode.TreeNode

// https://leetcode.com/problems/vertical-order-traversal-of-a-binary-tree/
object _987_VerticalOrderTraversalOfABinaryTree extends App {

  object Solution {

    case class Cord(row: Int, col: Int) {
      def left: Cord = copy(row = row + 1, col = col - 1)

      def right: Cord = copy(row = row + 1, col = col + 1)
    }

    def verticalTraversal(root: TreeNode): List[List[Int]] = {
      val markedTreeValues = traverse(root, Cord(0, 0), List.empty)

      markedTreeValues
        .sortBy(r => (r._1.col, r._1.row, r._2))
        .groupBy(_._1.col)
        .toList
        .sortBy(_._1)
        .map(_._2.map(_._2))

    }

    def traverse(
        tree: TreeNode,
        currCord: Cord,
        acc: List[(Cord, Int)]
    ): List[(Cord, Int)] = {
      val currNode = acc :+ (currCord -> tree.value)

      val leftTraverse =
        if (tree.left != null) traverse(tree.left, currCord.left, acc) else Nil
      val rightTraverse =
        if (tree.right != null) traverse(tree.right, currCord.right, acc)
        else Nil

      currNode ++ leftTraverse ++ rightTraverse
    }
  }

  val testTree1 = new TreeNode(
    3,
    new TreeNode(9),
    new TreeNode(20, new TreeNode(15), new TreeNode(7))
  )

  println(Solution.verticalTraversal(testTree1))

}
