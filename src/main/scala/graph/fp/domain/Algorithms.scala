package graph.fp.domain

object Algorithms {

  def dfs(nodes: List[Node])(graph: Graph): List[Node] =
    nodes match {
      case Nil => Nil
      case v :: vs =>
        graph.`match`(v) match {
          case (Some(c), g) => v :: dfs(c.suc ++ vs)(g)
          case (None, g)    => dfs(vs)(g)
        }
    }

}
