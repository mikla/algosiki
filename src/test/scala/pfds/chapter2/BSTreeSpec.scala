package pfds.chapter2

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import pfds.chapter2.BSTree.{Branch, Empty, Tree}

class BSTreeSpec extends AnyFlatSpec with should.Matchers {

  "A Tree.member()" should "return true if element in the tree" in {
    assert(testTree.member(7))
  }

  "A Tree.member()" should "return false if element is not in the tree" in {
    assert(!testTree.member(15))
  }

  "Tree.insert" should "insert value to binary tree" in {
    val tree = (1 to 9).foldLeft(Empty: Tree[Int]) { case (acc, elem) =>
      acc.insert(elem)
    }

    assert((1 to 9).forall(tree.member))
  }


  val testTree = Branch(9, Branch(8, Branch(7)), Branch(10, Branch(11), Branch(12)))

}
