package leetcode

object _653_TwoSums4 extends App {
  object Solution {

    val hs = new java.util.HashSet[Int]()

    def findTarget(root: TreeNode, k: Int): Boolean = {
      if (root == null) return false
      if (hs.contains(root.value)) return true
      hs.add(k - root.value)
      findTarget(root.right, k) || findTarget(root.left, k)
    }
  }

}
