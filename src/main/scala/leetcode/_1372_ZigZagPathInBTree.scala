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

    def loop(
        tree: FTree[Int],
        direction: Direction,
        depth: Int,
        maxDepth: Int
    ): Int =
      tree match {
        case t @ FNode(_, left, right) =>
          val maxCurDepth = Math.max(depth + 1, maxDepth)
          if (direction == Left)
            Math.max(
              loop(left, direction.reverse, depth + 1, maxCurDepth),
              loop(right, Left, 1, maxCurDepth)
            )
          else
            Math.max(
              loop(left, Right, 1, maxCurDepth),
              loop(right, direction.reverse, depth + 1, maxCurDepth)
            )

        case Empty =>
          Math.max(depth, maxDepth) - 1
      }

    loop(fpRTree, Left, 0, 0)
  }

}
