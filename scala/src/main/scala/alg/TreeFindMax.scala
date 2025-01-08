package alg

/** Find max sum path from top to bottom
  */

object TreeFindMax extends App {

  case class Milestone(points: Int, milestones: List[Milestone] = Nil)

  def findMax(m: Milestone): Int = {

    def loop(milestone: Milestone, currSum: Int): List[Int] =
      milestone match {
        case Milestone(points, Nil) =>
          List(currSum + points)
        case Milestone(points, milestones) =>
          milestones.flatMap(loop(_, currSum + points))
      }

    loop(m, 0).max

  }

  val test = Milestone(
    1,
    List(
      Milestone(1, List(Milestone(5), Milestone(10))),
      Milestone(3, List(Milestone(7), Milestone(5)))
    )
  )

  println(findMax(test))

}
