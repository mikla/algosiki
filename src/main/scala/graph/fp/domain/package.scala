package graph.fp

package object domain {

  type Adj = List[Arr]
  type Node = Int

  case class Arr(node: Node, label: String)

}
