package common

import common.tree.{FNode, FTree}
import leetcode.TreeNode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FTreeSpec extends AnyFlatSpec with should.Matchers {

  "FTree" should "fromTreeNode" in {

    val treeNode =
      new TreeNode(
        1,
        new TreeNode(2),
        new TreeNode(3, new TreeNode(4), new TreeNode(5))
      )

    val fTree = FTree.fromTreeNode(treeNode)

    fTree shouldBe FNode(1, FNode(2), FNode(3, FNode(4), FNode(5)))
  }

  "FTree" should "toTreeNode" in {

    val fTree = FNode(1, FNode(2), FNode(3, FNode(4), FNode(5)))

    val treeNode = FTree.toTreeNode(fTree)

    treeNode shouldBe treeNode
  }

  "FTree" should "keep invariant" in {

    val fTree = FNode(1, FNode(2), FNode(3, FNode(4), FNode(5)))

    val treeNode = FTree.toTreeNode(fTree)

    val fTree2 = FTree.fromTreeNode(treeNode)

    fTree shouldBe fTree2

  }

}
