package leetcode

import alg.DisjointSetApp.DisjointSet

object _990_SatisfiabilityOfEqualityEquations extends App {

  object Solution {
    def equationsPossible(equations: Array[String]): Boolean = {

      val allSet = equations.foldLeft(DisjointSet.empty[String]) {
        case (acc, el) =>
          acc.makeSet(el.head.toString).makeSet(el.last.toString)
      }

      val (eqs, nonEqs) = equations.partition {
        case s"$x==$y" => true
        case _         => false
      }

      val disJointEq =
        eqs.foldLeft(allSet) {
          case (acc, s"$x==$y") =>
            if (x != y)
              acc.union(x, y).map(_._2).getOrElse(acc)
            else acc
          case (acc, _) => acc
        }

      !nonEqs
        .exists { case s"$x!=$y" =>
          (for {
            (parentx, _) <- disJointEq.find(x)
            (parenty, _) <- disJointEq.find(y)
          } yield parentx == parenty).getOrElse(true)
        }
    }

  }

  println(Solution.equationsPossible(Array("a==b", "b!=a"))) // false
  println(Solution.equationsPossible(Array("b==a", "a==b"))) // true

  println(Solution.equationsPossible(Array("a!=b", "b!=c", "c!=a")))
  println(Solution.equationsPossible(Array("a==b", "e==c", "b==c", "a!=e")))
  println(Solution.equationsPossible(Array("c==c", "b==d", "x!=z"))) // true

}
