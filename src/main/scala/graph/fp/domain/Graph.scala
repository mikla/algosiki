package graph.fp.domain

import graph.fp.domain.Graph.{And, Context, Empty}

sealed trait Graph {

  def nodes: List[Node] = ufold[List[Node]](
    (ctx, acc) => ctx.node :: acc,
    List.empty
  )

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

  /** gmap via ufold impl */
  def gmap2(f: Context => Context): Graph =
    ufold[Graph]((ctx, acc) => And(f(ctx), acc), Graph.Empty)

  /** `u` stands for unordered */
  def ufold[C](f: (Context, C) => C, u: C): C = this match {
    case Graph.Empty        => u
    case Graph.And(c, rest) => f(c, rest.ufold(f, u))
  }

  def reverse: Graph = gmap(_.swap)

  def del(node: Node) = this match {
    case Graph.Empty         => Graph.Empty
    case And(context, graph) => ???
  }

  /** Searches for the context of a given node
    * @return
    *   context (if found) with remaining graph
    */
  def `match`(node: Node): Option[(Context, Graph)] = this match {
    case And(ctx @ Context(_, n, _, _), graph) if n == node =>
      Some((ctx, graph))
    case And(_, graph) => graph.`match`(node)
    case Empty         => None
  }

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

    /** Selects successors from a known context */
    def suc: List[Arr] = out
  }

  implicit class ContextOps(context: Context) {
    def &(graph: Graph): Graph = Graph.And(context, graph)
  }

}
