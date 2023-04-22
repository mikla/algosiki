package leetcode

import common.tree.{Empty, FNode, FTree}

class _1372_ZigZagPathInBTree extends App {

  sealed trait Direction {
    def reverse: Direction
  }
  case object Left extends Direction {
    override def reverse: Direction = Right
  }
  case object Right extends Direction {
    override def reverse: Direction = Left
  }

  def longestZigZag(root: TreeNode): Int = {
    val fpRTree = FTree.fromTreeNode(root)

    def loop(tree: FTree[Int], direction: Direction, depth: Int): Int =
      tree match {
        case t @ FNode(Empty, _, right: FNode[Int]) =>
          if (direction == Left) loop(right, direction.reverse, depth + 1)
          else loop(right, direction.reverse, depth)

        case t @ FNode(left: FNode[Int], _, Empty)                 =>
          if (direction == Right) loop(left, direction.reverse, depth + 1)
          else loop(left, direction.reverse, depth)

        case t @ FNode(left: FNode[Int], value, right: FNode[Int]) =>
          loop(left, direction.reverse, depth + 1)
          loop(right, direction.reverse, depth)

          if (direction == Right) loop(left, direction.reverse, depth + 1)
          else loop(right, direction.reverse, depth)


        case FNode(Empty, _, Empty)                                => depth

        case Empty => depth
      }

    Math.max(loop(fpRTree, Left, 0), loop(fpRTree, Right, 0))
  }

}
