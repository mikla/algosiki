package leetcode

object _429_NAryTreeLevelOrderTraversal extends App {

  object Solution {
    def levelOrder(root: Node): List[List[Int]] = {

      def loop(level: Int, tree: Node): List[(Int, Int)] = {
        val current = List(level -> tree.value)

        val children = tree.children.flatMap(loop(level + 1, _))

        current ++ children
      }

      if (root != null) {
        loop(0, root).groupBy(_._1).toList.sortBy(_._1).map(_._2.map(_._2))
      } else List.empty
    }

  }

  Solution.levelOrder(new Node(0))

}
