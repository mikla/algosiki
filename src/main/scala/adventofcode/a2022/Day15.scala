package adventofcode.a2022

import adventofcode.common.Point
import adventofcode.common.Point.{PointI, PointL}
import helpers.Input
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

object Day15 extends App {

  val input = Input.readListString("src/main/scala/adventofcode/a2022/Day15.in")

  val points = input.map {
    case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
      Point(sensorX.toInt, sensorY.toInt) -> Point(
        beaconX.toInt,
        beaconY.toInt
      )
  }

  val targetY = 2000000

  def collectClosestPoints(
      sensor: PointI,
      beacon: PointI
  ): Task[ArrayBuffer[PointI]] = Task.eval {
    val dist = sensor.manh(beacon)

    println(
      s"sensor: $sensor, beacon: $beacon, dist: $dist" + Thread
        .currentThread()
        .getName
    )

    val vec = scala.collection.mutable.ArrayBuffer.empty[PointI]

    var x0 = sensor.x - dist

    Scheduler.Implicits.global.scheduleAtFixedRate(10.second, 1.minute) {
      println(s"x: $x0, left: ${sensor.x + dist - x0}, for $sensor")
      println("=====================")
    }

    while (x0 <= sensor.x + dist) {
      var y0 = sensor.y - dist
      while (y0 <= sensor.y + dist) {
        val point = Point(x0, y0)
        if (
          point.manh(
            sensor
          ) <= dist && point != sensor && point != beacon && point.y == targetY
        )
          vec.addOne(point)
        y0 += 1
      }
      x0 += 1
    }

    println(s"Point $sensor Finished!")
    vec
  }

  val part1 = Task
    .parTraverseN(8)(points) { case (s, b) =>
      collectClosestPoints(s, b)
    }
    .map(_.reduce(_ ++ _))
    .map(_.distinct.count(_.y == targetY))

  println(Await.result(part1.runToFuture, Duration.Inf))

}
