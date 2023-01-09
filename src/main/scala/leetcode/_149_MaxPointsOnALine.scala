package leetcode

object _149_MaxPointsOnALine extends App {

  def maxPoints(points: Array[Array[Int]]): Int = {

    if (points.length == 1) 1
    else {

      var i = 0;
      var max = 0;
      while (i < points.length) {
        var j = i + 1;
        val Array(p1x, p1y) = points(i)
        while (j < points.length) {
          val Array(p2x, p2y) = points(j)

          val lineEquation = lineEq((p1x, p1y), (p2x, p2y)) _
          val pointsOnLine = points.count { case Array(x, y) =>
            lineEquation((x, y))
          }

          if (pointsOnLine > max) max = pointsOnLine

          j += 1
        }

        i += 1
      }

      max
    }
  }

  def lineEq(p1: (Int, Int), p2: (Int, Int))(px: (Int, Int)): Boolean =
    (px._1 - p1._1) * (p2._2 - p1._2) == (px._2 - p1._2) * (p2._1 - p1._1)

}
