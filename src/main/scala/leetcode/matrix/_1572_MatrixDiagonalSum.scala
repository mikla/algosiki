package leetcode.matrix

object _1572_MatrixDiagonalSum extends App {

  def diagonalSum(mat: Array[Array[Int]]): Int = {
    (mat.indices.map(i => (i, i)) ++
      mat.indices.zip(mat.indices.reverse)).distinct.map { case (row, col) =>
      mat(row)(col)
    }.sum
  }

  val test = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))

  println(diagonalSum(test))

}
