package graph.fp.domain

import graph.fp.domain.Graph._

import scala.language.postfixOps

object GraphExamples extends App {

  val fig1 =
    Context(List(Arr(2, "left"), Arr(3, "up")), 1, "a", List(Arr(2, "right"))) &
      (Context(Nil, 2, "b", List(Arr(3, "down"))) &
        (Context(Nil, 3, "c", Nil) & Empty))

  println(Renderer.renderDot(fig1))
  println(Renderer.renderDot(fig1.reverse))

  println(fig1.nodes)
  println(fig1.`match`(3)) // Some((Context(List(),3,c,List()),Empty))
  println(fig1.`match`(2)) // Some((Context(List(),3,c,List()),Empty))
  println(fig1.`match`(5)) // None

}
