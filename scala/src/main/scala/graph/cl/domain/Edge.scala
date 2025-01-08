package graph.cl.domain

/** Note: for undirected we have to store 2 Edges: A -> B and B -> A
  */
case class Edge(from: Node, to: Node, weight: Weight)
