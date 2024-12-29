package graph.fp.domain

object Renderer {

  def renderNode(context: Graph.Context): String =
    s"""  ${context.node} [label="${context.label}"];"""

  def renderEdges(context: Graph.Context): String = {
    val inEdges =
      context.in
        .map(arr =>
          s"""${arr.node} -> ${context.node} [label="${arr.label}"];"""
        )
        .mkString("\n")
    val outEdges = context.out
      .map(arr => s"""${context.node} -> ${arr.node} [label="${arr.label}"];""")
      .mkString("\n")
    s"$inEdges\n$outEdges"
  }

  def renderGraph(graph: Graph, acc: String): String = graph match {
    case Graph.Empty => acc
    case Graph.And(context, subgraph) =>
      val nodeStr = renderNode(context)
      val edgesStr = renderEdges(context)
      renderGraph(subgraph, s"$acc\n$nodeStr\n$edgesStr")
  }

  def renderDot(graph: Graph): String = {
    s"digraph G {\n${renderGraph(graph, "")}\n}"
  }

}
