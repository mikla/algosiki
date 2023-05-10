package leetcode.matrix

object _59_SpiralMatrix2 extends App {

  import scala.collection.mutable

  def generateMatrix(n: Int): Array[Array[Int]] = {

    // 1  2  3  4
    // 5  6  7  8
    // 9 10 11 12

    val matrix = Array.ofDim[Int](n, n)

    var boundLeft = 0
    var boundRight = n - 1
    var boundTop = 0
    var boundBottom = n - 1

    val points = mutable.Set.empty[(Int, Int)]
    for {
      row <- (1 until n)
      col <- (1 until n)
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

    var x = 1
    while (points.nonEmpty) {
      val p1 = goRightPoints(boundTop, boundLeft, boundRight)
      val p2 = goDown(boundRight, boundTop, boundBottom)

      boundRight -= 1
      val p3 = goLeftPoints(boundBottom, boundRight, boundLeft)

      boundTop += 1
      val p4 = goUp(boundLeft, boundBottom, boundTop)

      (p1 ++ p2 ++ p3 ++ p4).distinct.foreach { case p @ (row, col) =>
        matrix(row)(col) = x
        x += 1
        points.remove(p)
      }

      boundLeft += 1
      boundBottom -= 1
    }



    matrix
  }

  println(generateMatrix(3).toList.map(_.toList))

}
