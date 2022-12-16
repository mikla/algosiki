package adventofcode.a2022

import helpers.Input

object Day8 extends App {

  val in = Input
    .readListString("src/main/scala/adventofcode/a2022/Day8.in")
    .map(_.toArray.map(_.toString.toInt))

  val MaxX = in.size
  val MaxY = in.head.length

  def isVisible(x: Int, y: Int) = {
    val elem = in(x)(y)

    val visibleFromRight = {
      val y0 = y + 1
      (y0 until MaxX).forall(y1 => in(x)(y1) < elem)
    }

    val visibleFromLeft = {
      val y0 = y - 1
      (0 to y0).forall(y1 => in(x)(y1) < elem)
    }

    val visibleFromTop = {
      val x0 = x - 1
      (0 to x0).forall(x1 => in(x1)(y) < elem)
    }

    val visibleFromBottom = {
      val x0 = x + 1
      (x0 until MaxX).forall(x1 => in(x1)(y) < elem)
    }

    visibleFromTop || visibleFromLeft || visibleFromRight || visibleFromBottom
  }

  def isInField(x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && x < MaxX && y <= MaxY

  def scenicScore(x: Int, y: Int): Int = {
    val elem = in(x)(y)

    val visibleFromRight = {
      val y0 = y + 1
      (y0 until MaxX)
        .collectFirst {
          case y1 if in(x)(y1) >= elem => y1
        }
        .map(yLast => Math.abs(yLast - y))
        .getOrElse(MaxY - y - 1)
    }

    val visibleFromLeft = {
      val y0 = y - 1

      (y0 to 0 by (-1))
        .collectFirst {
          case y1 if in(x)(y1) >= elem => y1
        }
        .map(yLast => Math.abs(yLast - y))
        .getOrElse(y)

    }

    val visibleFromTop = {
      val x0 = x - 1
      (x0 to 0 by (-1))
        .collectFirst {
          case x1 if in(x1)(y) >= elem => x1
        }
        .map(xLast => Math.abs(xLast - x))
        .getOrElse(x)
    }

    val visibleFromBottom = {
      val x0 = x + 1
      (x0 until MaxX)
        .collectFirst {
          case x1 if in(x1)(y) >= elem => x1
        }
        .map(xLast => Math.abs(xLast - x))
        .getOrElse(MaxX - x - 1)
    }

    visibleFromTop * visibleFromLeft * visibleFromRight * visibleFromBottom
  }

  { // part 1
    var x0 = 1
    var visible = 0
    while (x0 < MaxX - 1) {
      var y0 = 1
      while (y0 < MaxY - 1) {
        if (isVisible(x0, y0)) visible += 1
        y0 += 1
      }
      x0 += 1
    }

    println(visible + ((MaxY + MaxX) * 2 - 4))
  }

  { // part 2
    var x0 = 0
    var max = 0
    while (x0 < MaxX) {
      var y0 = 0
      while (y0 < MaxY) {
        val score = scenicScore(x0, y0)
        if (score > max) max = score
        y0 += 1
      }
      x0 += 1
    }

    println(max)
  }

}
