package pfds.chapter3

import pfds.chapter3.LeftistHeap.Heap.{insert, merge}

object LeftistHeap {

  trait Heap[+T] {

    def ins[V >: T](elem: V)(implicit O: Ordering[V]): Heap[V] =
      insert(elem, this)

    def findMin: T = this match {
      case Branch(_, e, _, _) => e
      case Empty => throw new RuntimeException("findMin on Empty heap.")
    }

    def deleteMin[V >: T](implicit O: Ordering[V]): Heap[V] = this match {
      case Branch(_, _, left, right) => merge[V](left, right)
      case Empty => throw new RuntimeException("deleteMin on Empty heap.")
    }

  }

  object Heap {

    // Exercise 3.3
    // Not really efficient impl.
    def fromList[T](list: List[T])(implicit O: Ordering[T]): Heap[T] =
      list.foldLeft(Empty: Heap[T]) { case (heap, element) =>
        heap.ins(element)
      }

    def print[T](h: Heap[T]): String = {

      def loop(heap: Heap[T], level: Int): List[String] = heap match {
        case Branch(rank, elem, left, right) =>
          val c = " ".repeat(rank * 2) + s"($elem)" + " ".repeat(rank * 2)
          val leftDel = " ".repeat(rank * 2) + "/ " + " ".repeat(rank * 2) + "\\"

          val l = loop(left, level + 1)
          val r = loop(right, level +1)
          c :: leftDel :: l.zip(r).map { case (l1, r1) => l1 + r1 }

        case Empty => List("  E  ")
      }

      loop(h, 0).mkString("\n")

    }

    def merge[T](h1: Heap[T], h2: Heap[T])(implicit O: Ordering[T]): Heap[T] =
      (h1, h2) match {
        case (Empty, h) => h
        case (h, Empty) => h
        case (
              h1 @ Branch(_, x, a1, b1),
              h2 @ Branch(_, y, a2, b2)
            ) =>
          if (O.lteq(x, y)) makeT(x, a1, merge(b1, h2))
          else makeT(y, a2, merge(h1, b2))
      }

    def insert[T](elem: T, h1: Heap[T])(implicit O: Ordering[T]): Heap[T] =
      merge(Branch(1, elem, Empty, Empty), h1)

    // swaps children of node base on rank if necessary
    private def makeT[T](elem: T, a: Heap[T], b: Heap[T]): Heap[T] =
      if (rank(a) >= rank(b)) Branch(rank(b) + 1, elem, a, b)
      else Branch(rank(a) + 1, elem, b, a)

    private def rank[T](node: Heap[T]): Int = node match {
      case Branch(rank, _, _, _) => rank
      case Empty                 => 0
    }

  }

  case object Empty extends Heap[Nothing]

  case class Branch[T](
      rank: Int,
      elem: T,
      left: Heap[T] = Empty,
      right: Heap[T] = Empty
  ) extends Heap[T]

}
