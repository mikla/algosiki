package alg

import alg.DisjointSetApp.DisjointSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IDisjointSetSpec extends AnyFlatSpec with should.Matchers {

  "DisjountSet.fromList" should "return DisjointSet" in {

    val ds = DisjointSet.fromList(List("a", "b", "c", "d"))

    val abUn = DisjointSet.dunion("a", "b", ds)

    println(ds)
    println(abUn)

    println(abUn.flatMap(_.find("a")))
    println(abUn.flatMap(_.find("b")))
  }

}
