package pfds.chapter3

/** Issue: binary search tree works well on random data, however, it has poor
  * performance on sorted data and might take O(n). Red-black tree shows O(log
  * n) for any individual operation.
  */
object RedBlackTree {

  trait Color
  object Color {
    case object Red extends Color
    case object Black extends Color
  }

  trait RBTree[+T]
  case class Branch[T](
      color: Color,
      left: Branch[T],
      elem: T,
      branch: Branch[T]
  ) extends RBTree[T]
  case object Empty extends RBTree[Nothing]

}
