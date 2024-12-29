package graph.cl.domain

case class Node(
    id: NodeId,
    edges: Map[Node, Edge] = Map.empty,
    label: Option[String] = None
) {

  def numEdges: Int = edges.values.size

  def getEdge(neighbor: Node): Option[Edge] =
    edges.get(neighbor)

  def addEdge(neighbor: Node, weight: Weight) =
    copy(edges = edges + (neighbor -> Edge(this, neighbor, weight)))

  def removeEdge(neighbor: Node) =
    copy(edges = edges.removed(neighbor))

  def getSortedEdgeList() = ???

}
