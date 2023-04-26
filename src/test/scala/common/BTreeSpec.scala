package common

import common.tree.{BNode, BTree}
import leetcode.TreeNode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BTreeSpec extends AnyFlatSpec with should.Matchers {

  "FTree" should "fromTreeNode" in {

    val treeNode =
      new TreeNode(
        1,
        new TreeNode(2),
        new TreeNode(3, new TreeNode(4), new TreeNode(5))
      )

    val fTree = BTree.fromTreeNode(treeNode)

    fTree shouldBe BNode(1, BNode(2), BNode(3, BNode(4), BNode(5)))
  }

  "FTree" should "toTreeNode" in {

    val fTree = BNode(1, BNode(2), BNode(3, BNode(4), BNode(5)))

    val treeNode = BTree.toTreeNode(fTree)

    treeNode shouldBe treeNode
  }

  "FTree" should "keep invariant" in {

    val fTree = BNode(1, BNode(2), BNode(3, BNode(4), BNode(5)))

    val treeNode = BTree.toTreeNode(fTree)

    val fTree2 = BTree.fromTreeNode(treeNode)

    fTree shouldBe fTree2

  }

}
