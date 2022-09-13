package pfds.chapter3

object LeftistHeap {

  trait Heap[+T]
  case object Empty extends Heap[Nothing]
  case class Leaf[T](elem: T, left: Heap[T], right: Heap[T]) extends Heap[T]



}
