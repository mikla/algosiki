package leetcode.matrix

object _54_SpiralMatrix extends App {

  import scala.collection.mutable

  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {

    // 1  2  3  4
    // 5  6  7  8
    // 9 10 11 12

    var boundLeft = 0
    var boundRight = matrix.head.length - 1
    var boundTop = 0
    var boundBottom = matrix.length - 1

    val points = mutable.Set.empty[(Int, Int)]
    for {
      row <- matrix.indices
      col <- matrix.head.indices
    } {
      points.add((row, col))
    }

    def goRightPoints(row: Int, colStart: Int, colEnd: Int): Seq[(Int, Int)] =
      for (col <- colStart to colEnd) yield (row, col)

    def goDown(col: Int, rowStart: Int, rowEnd: Int): Seq[(Int, Int)] =
      for (row <- rowStart to rowEnd) yield (row, col)

    def goLeftPoints(row: Int, colStart: Int, colEnd: Int): Seq[(Int, Int)] =
      for (col <- colStart to colEnd by (-1)) yield (row, col)

    def goUp(col: Int, rowStart: Int, rowEnd: Int): Seq[(Int, Int)] =
      for (row <- rowStart to rowEnd by (-1)) yield (row, col)

    val ans = mutable.ArrayBuffer.empty[Int]

    while (points.nonEmpty) {
      val p1 = goRightPoints(boundTop, boundLeft, boundRight)
      val p2 = goDown(boundRight, boundTop, boundBottom)

      boundRight -= 1
      val p3 = goLeftPoints(boundBottom, boundRight, boundLeft)

      boundTop += 1
      val p4 = goUp(boundLeft, boundBottom, boundTop)

      (p1 ++ p2 ++ p3 ++ p4).distinct.foreach { case p @ (row, col) =>
        ans.addOne(matrix(row)(col))
        points.remove(p)
      }

      boundLeft += 1
      boundBottom -= 1
    }

    ans.toList
  }

  val test = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  val test2 = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12))

  println(spiralOrder(test))
  println(spiralOrder(test2))

}
