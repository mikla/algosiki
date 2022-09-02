package leetcode

// 637 https://leetcode.com/problems/average-of-levels-in-binary-tree/
object AverageOfLevelsInBinaryTree extends App {

  object Solution {
    def averageOfLevels(root: TreeNode): Array[Double] = {

      val mapOfSums = loop(0, root)
        .groupBy(_._1)
        .view
        .mapValues(v => avg(v.map(_._2)))
        .toMap

      val result = new Array[Double](mapOfSums.keys.size)

      mapOfSums.foreach { case (k, v) =>
        result(k) = v
      }

      result
    }

    private def avg(l: List[Double]) = {
      val size = l.size
      l.sum / size
    }

    def loop(level: Int, tree: TreeNode): List[(Int, Double)] = {
      val currentLevel = List(level -> tree.value.toDouble)
      val searchLeft = if (tree.left != null) loop(level + 1, tree.left) else Nil
      val searchRight = if (tree.right != null) loop(level + 1, tree.right) else Nil

      currentLevel ++ searchLeft ++ searchRight
    }
  }

  val testTree1 = new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))

  println(Solution.averageOfLevels(testTree1).mkString(","))

}
