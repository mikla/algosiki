package adventofcode.a2022

import helpers.Input

object Day17 extends App {

  case class Screen(initWidth: Int = 7, initHeight: Int = 4) {

    var screen = Array.fill[Char](initHeight, initWidth)('.')

    def highest: Option[Int] =
      screen
        .map(_.zipWithIndex)
        .zipWithIndex
        .filter(_._1.exists(_._1 == '#'))
        .maxByOption(_._2)
        .map(_._2)

    var currentHeight = initHeight - 1

    def drawSeq(points: List[(Int, Int)]) =
      points.foreach { case (x, y) =>
        drawPixel(x, y)
      }

    def drawPixel(x: Int, y: Int): Unit =
      screen(x)(y) = '#'

    def erasePixel(x: Int, y: Int): Unit =
      screen(x)(y) = '.'

    def extendScreenBy(x: Int): Unit = {
      if (currentHeight + x < Integer.MAX_VALUE) {
        screen = screen ++ Array.fill[Char](x, initWidth)('.')
        currentHeight = currentHeight + x
      }
    }

    def drawScreen: Unit = {
      screen.map(_.mkString("")).reverse.foreach(println(_))
    }

    def isBlocked(points: List[(Int, Int)]) =
      points.exists { case (x, y) =>
        screen(x)(y) == '#'
      }

    def compact: Option[Int] = {
      val possibleToCompact = screen.zipWithIndex.reverse.collectFirst {
        case (line, index) if line.mkString("") == "#######" => index
      }

      possibleToCompact.foreach { rows =>
        screen = screen.drop(rows)
      }

      possibleToCompact
    }

  }

  trait Figure

  object Figure {

    def allWithingTheScreen(points: List[(Int, Int)]) =
      points.forall { case (x, y) =>
        y >= 0 && y < 7 && x >= 0
      }

    def withScreenCheck(
        points: List[(Int, Int)]
    )(newPoints: List[(Int, Int)]) = {
      if (allWithingTheScreen(newPoints)) newPoints else points
    }

    def moveRight(points: List[(Int, Int)]) = withScreenCheck(points) {
      points.map { case (x, y) =>
        (x, y + 1)
      }
    }

    def reachedBottom(points: List[(Int, Int)]) =
      points.exists { case (x, y) =>
        x == 0
      }

    def moveLeft(points: List[(Int, Int)]) = withScreenCheck(points) {
      points.map { case (x, y) =>
        (x, y - 1)
      }
    }

    def moveDown(points: List[(Int, Int)]) = withScreenCheck(points) {
      points.map { case (x, y) =>
        (x - 1, y)
      }
    }

    def minus(initRow: Int): List[(Int, Int)] =
      List((initRow, 2), (initRow, 3), (initRow, 4), (initRow, 5))

    def plus(initRow: Int): List[(Int, Int)] =
      List(
        (initRow, 3),
        (initRow - 1, 2),
        (initRow - 1, 3),
        (initRow - 1, 4),
        (initRow - 2, 3)
      )

    def L(initRow: Int): List[(Int, Int)] =
      List(
        (initRow, 4),
        (initRow - 1, 4),
        (initRow - 2, 4),
        (initRow - 2, 3),
        (initRow - 2, 2)
      )

    def I(initRow: Int): List[(Int, Int)] =
      (0 to 3).map(x => (initRow - x, 2)).toList

    def square(initRow: Int): List[(Int, Int)] =
      List(
        (initRow, 2),
        (initRow, 3),
        (initRow - 1, 2),
        (initRow - 1, 3)
      )

    val AllFigures: Seq[(Int => List[(Int, Int)], Int)] =
      List((minus, 1), (plus, 3), (L, 3), (I, 4), (square, 2))

  }

  // 0 .. ?
  val instructions =
    Input.readAsString("src/main/scala/adventofcode/a2022/Day17.in")
  val instructionsTotal = instructions.length

  val screen = Screen()

  var figureCounter: Long = 0
  val MaxRockCounter: Long = 2022

  var operationPosition = 0

  def incrementOperationPos() = {
    if ((operationPosition + 1) == instructions.length) operationPosition = 0
    else operationPosition += 1
  }

  var compacted = 0

  while (figureCounter <= MaxRockCounter) {

    println(s"Figure flying: $figureCounter")

    val (figurePointsFn, figureHeight) =
      Figure.AllFigures((figureCounter % Figure.AllFigures.length).toInt)

    val currentTop =
      screen.highest
        .map(_ + 3 + figureHeight)
        .getOrElse(3)

    var figurePoints = figurePointsFn(currentTop)

    var rockIsFallinDown = true
    while (rockIsFallinDown) {
      val operation = instructions(operationPosition % instructions.length)

      // jet
      operation match {
        case '>' =>
          val possiblePointAfterMove = Figure.moveRight(figurePoints)
          if (!screen.isBlocked(possiblePointAfterMove)) {
            figurePoints = possiblePointAfterMove
          }
          incrementOperationPos()

        case '<' =>
          val possiblePointAfterMove = Figure.moveLeft(figurePoints)
          if (!screen.isBlocked(possiblePointAfterMove)) {
            figurePoints = possiblePointAfterMove
          }
          incrementOperationPos()
      }

      // falling one step down
      val possiblePointAfterMove = Figure.moveDown(figurePoints)
      if (screen.isBlocked(possiblePointAfterMove)) {
        rockIsFallinDown = false
        screen.drawSeq(figurePoints)
      } else if (Figure.reachedBottom(possiblePointAfterMove)) {
        rockIsFallinDown = false

        // read one more operation
        // TODO remove duplication
        val operation = instructions(operationPosition)

        // jet
        operation match {
          case '>' =>
            val possiblePointAfterMoveFinal =
              Figure.moveRight(possiblePointAfterMove)
            if (!screen.isBlocked(possiblePointAfterMoveFinal)) {
              figurePoints = possiblePointAfterMoveFinal
            }
            incrementOperationPos()

          case '<' =>
            val possiblePointAfterMoveFinal =
              Figure.moveLeft(possiblePointAfterMove)
            if (!screen.isBlocked(possiblePointAfterMoveFinal)) {
              figurePoints = possiblePointAfterMoveFinal
            }
            incrementOperationPos()
        }

        screen.drawSeq(figurePoints)
      } else {
        figurePoints = possiblePointAfterMove
      }

    }

    screen.extendScreenBy(4)

    screen.compact.foreach { c =>
      compacted += c
    }

    figureCounter += 1
  }

//  screen.drawSeq(Figure.moveDown(Figure.moveDown(Figure.minus(3))))
//  screen.drawSeq(List((1, 1), (1, 0), (1, 6)))
//  println(screen.compact)

//  println(screen.highest)

  screen.drawScreen
  println(screen.highest.map(_ - 1).getOrElse(0) + compacted)

}
