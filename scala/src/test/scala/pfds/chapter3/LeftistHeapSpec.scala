package pfds.chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import pfds.chapter3.LeftistHeap.{Branch, Empty, Heap}

class LeftistHeapSpec extends AnyFlatSpec with should.Matchers {

  "Heap.fromList" should "make a heap" in {

    val heap = Heap.fromList((1 to 10).toList)

    val tree = Branch(
      rank = 3,
      elem = 1,
      left =
        Branch(2, 3, Branch(1, 4, Empty, Empty), Branch(1, 5, Empty, Empty)),
      right = Branch(
        rank = 2,
        elem = 2,
        left =
          Branch(2, 7, Branch(1, 8, Empty, Empty), Branch(1, 9, Empty, Empty)),
        right = Branch(1, 6, Branch(1, 10, Empty, Empty), Empty)
      )
    )

    println(Heap.print(heap))

    assert(heap == tree)
  }

}
