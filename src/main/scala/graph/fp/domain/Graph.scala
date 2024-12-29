package graph.fp.domain

sealed trait Graph

object Graph {

  case object Empty extends Graph
  case class And(val context: Context, val graph: Graph) extends Graph

  case class Arr(node: Int, label: String)

  type Adj = List[Arr]

  case class Context(
      in: Adj,
      node: Int,
      label: String,
      out: Adj
  )

  implicit class ContextOps(context: Context) {
    def &(graph: Graph): Graph = Graph.And(context, graph)
  }

}
