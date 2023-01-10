package leetcode

object _944_DeleteColumnsToMakeSorted extends App {

  def minDeletionSize(strs: Array[String]): Int = {
    val collen = strs.head.length

    (0 until collen)
      .map { i =>
        strs.map(_.charAt(i)).mkString("")
      }
      .filterNot(s => s.sorted == s)
      .length
  }

  println(minDeletionSize(Array("cba","daf","ghi")))
  println(minDeletionSize(Array("a","b")))
  println(minDeletionSize(Array("zyx","wvu","tsr")))

}
