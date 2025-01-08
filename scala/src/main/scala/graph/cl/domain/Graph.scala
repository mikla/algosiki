package graph.cl.domain

case class Graph(
    nodes: List[Node],
    undirected: Boolean
) {

  def numNodes = nodes.size

}
