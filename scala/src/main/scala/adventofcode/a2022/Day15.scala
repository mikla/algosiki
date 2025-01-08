package adventofcode.a2022

import adventofcode.common.Point
import adventofcode.common.Point.PointI
import helpers.Input

object Day15 extends App {

  val input =
    Input.readListString("src/main/scala/adventofcode/a2022/Day15_0.in")

  val points = input.map {
    case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
      val s = Point(sensorX.toInt, sensorY.toInt)
      val b = Point(
        beaconX.toInt,
        beaconY.toInt
      )

      (s, b, s.manh(b))
  }

  val targetY = 2000000

  def collectOnlyInQuestion(targetY: Int) = {

    val vec = scala.collection.mutable.ArrayBuffer.empty[PointI]

    var x0 = 0
    val goal = 20

    while (x0 <= goal) {
      val pq = Point(x0, targetY)

      val hasCloser = points.find { case ((sensor, beacon, distance)) =>
        pq.manh(sensor) <= distance && pq != sensor && pq != beacon
      }

      if (hasCloser.isEmpty) {
        println(s"Adding point $pq")
        vec.addOne(pq)
      }

      x0 += 1
    }

    vec
  }

  var y0 = 0
  while (y0 <= 20) {
    collectOnlyInQuestion(y0)
    y0 += 1
  }

//  println(collectOnlyInQuestion(targetY).toList.length)

}
