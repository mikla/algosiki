package pfds.chapter2

import scala.annotation.tailrec

object BSTree {

  sealed trait Tree[+T] {

    def member[V >: T](target: V)(implicit O: Ordering[V]): Boolean = {

      @tailrec
      def loop(tree: Tree[T]): Boolean = tree match {
        case Empty => false
        case Branch(value, left, right) =>
          if (O.lt(target, value)) loop(left)
          else if (O.gt(target, value)) loop(right)
          else true
      }

      loop(this)
    }

    def insert[V >: T](x: V)(implicit O: Ordering[V]): Tree[V] = {

      def loop(tree: Tree[V]): Tree[V] = tree match {
        case Empty => Branch(x)
        case s@Branch(value, left, right) =>
          if (O.lt(x, value)) Branch(value, loop(left), right)
          else if (O.gt(x, value)) Branch(value, left, loop(right))
          else s
      }

      loop(this)

    }


  }

  case object Empty extends Tree[Nothing]

  case class Branch[T](value: T, left: Tree[T] = Empty, right: Tree[T] = Empty) extends Tree[T]


}
