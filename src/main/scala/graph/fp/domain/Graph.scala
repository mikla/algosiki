package graph.fp.domain

import graph.fp.domain.Graph.{And, Context}

sealed trait Graph {

  def nodes: List[Node] = this match {
    case Graph.Empty               => Nil
    case Graph.And(context, graph) => ???
  }

  def isEmpty: Boolean = this match {
    case Graph.Empty => true
    case _           => false
  }

  /** Note that gmap preserves the structure of the nodes, but not necessarily
    * of the edged.
    */
  def gmap(f: Context => Context): Graph = this match {
    case Graph.Empty => Graph.Empty
    case Graph.And(context, graph) =>
      And(f(context), graph.gmap(f))
  }

  def reverse: Graph = gmap(_.swap)

}

object Graph {

  case object Empty extends Graph
  case class And(context: Context, graph: Graph) extends Graph

  case class Context(
      in: Adj,
      node: Node,
      label: String,
      out: Adj
  ) {
    def swap: Context = Context(out, node, label, in)
  }

  implicit class ContextOps(context: Context) {
    def &(graph: Graph): Graph = Graph.And(context, graph)
  }

}
