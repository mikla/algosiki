package leetcode

object _590_NaryTreePostorderTraversal extends App {

  def postorder(root: Node): List[Int] = {
    if (root != null) {
      root.children.flatMap(postorder) ++ List(root.value)
    } else List.empty[Int]
  }

}
