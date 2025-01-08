package graph.fp.domain

object helpers {

  def newNodes[A, B](i: Int, g: Graph): List[Node] = {
    val n = g.nodes.foldLeft(0)(math.max)
    (n + 1 to n + i).toList
  }

}
