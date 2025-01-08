package benchmark.tree

import org.openjdk.jmh.annotations.{Benchmark, Scope, Setup, State}
import pfds.chapter3.LeftistHeap

@State(Scope.Benchmark)
class LeftistTreeFromList {

  val itemsCount = 10000

  var items: List[Int] = _

  @Setup
  def setup(): Unit = {
    items = (1 to itemsCount).toList
  }

  @Benchmark
  def fromListFP =
    LeftistHeap.Heap.fromList(items)

}
