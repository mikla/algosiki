package leetcode

import leetcode._37_SudokuSolver.Cell.{Filled, Given}
import adventofcode.a2022.Day5.sol
import leetcode._37_SudokuSolver.Coord.box

object _37_SudokuSolver extends App {

  case class Coord(x: Int, y: Int) {
    assert(x > 0 && x < 10 && y > 0 & y < 10)
  }

  object Coord {
    val boxesCoordStart: Map[Int, Coord] = (1 to 9).map { boxId =>
      val x = ((boxId - 1) % 3) * 3 + 1
      val y = ((boxId - 1) / 3) * 3 + 1
      boxId -> Coord(x, y)
    }.toMap

    val boxCoordinatesCache: IndexedSeq[(Int, Set[Coord])] =
      (1 to 9).map { boxId =>
        boxId -> box(boxId)
      }

    def box(index: Int): Set[Coord] = {
      val start = boxesCoordStart(index)
      (for {
        x <- (start.x until start.x + 3).toList
        y <- (start.y until start.y + 3).toList
      } yield Coord(x, y)).toSet
    }

  }

  trait Cell
  object Cell {
    case class Given(value: Int) extends Cell
    case class Possible(values: Set[Int]) extends Cell
    case class Filled(value: Int) extends Cell
    case object Empty extends Cell
  }

  case class Box(map: Map[Coord, Cell]) {
    val singlePossible = map.collect {
      case (k, v @ Cell.Possible(values)) if values.size == 1 =>
        k -> values.head
    }.toList

    lazy val allowedValues: Set[Int] = {
      val boxValus = map.view.values.collect {
        case Cell.Given(v)  => v
        case Cell.Filled(v) => v
      }
      if (boxValus.toList.distinct.size == boxValus.size) {
        Puzzle.allAllowedValues diff boxValus.toSet
      } else Set.empty
    }

    val unfilled: List[(Coord, Cell)] =
      map.collect { case (k, v @ (Cell.Empty | Cell.Possible(_))) =>
        k -> v
      }.toList

    val possibleCells: List[(Coord, Cell.Possible)] =
      map.collect { case (k, p @ Cell.Possible(_)) =>
        k -> p
      }.toList

  }

  case class Puzzle(map: Map[Coord, Cell]) {

    val unfilledCount = 
      map.view.values.count {
        case ((_: Cell.Filled) | (_: Cell.Given)) => false
        case _                                    => true
      }

    def isSolved: Boolean = unfilledCount == 0


    val boxes: IndexedSeq[Box] =
      Coord.boxCoordinatesCache
        .map { case (_, coords) =>
          Box(map.filter { case (k, _) =>
            coords.contains(k)
          })
        }

    def allowedInRowFor(coord: Coord): Set[Int] = {
      val cellValues = (1 to 9)
        .map(x => Coord(x, coord.y))
        .map(map(_))
        .collect {
          case Filled(v) => v
          case Given(v)  => v
        }

      if (cellValues.distinct.size == cellValues.size)
        Puzzle.allAllowedValues diff cellValues.toSet
      else Set.empty
    }

    def allowedInColFor(coord: Coord): Set[Int] = {
      val cellValues = (1 to 9)
        .map(y => Coord(coord.x, y))
        .map(map(_))
        .collect {
          case Filled(v) => v
          case Given(v)  => v
        }

      if (cellValues.distinct.size == cellValues.size)
        Puzzle.allAllowedValues diff cellValues.toSet
      else Set.empty
    }

    def update(coord: Coord, value: Cell) =
      this.copy(map = map.updated(coord, value))
  }

  object Puzzle {

    val allAllowedValues: Set[Int] = (1 to 9).toSet

    def allowedValuesForCell(
        allowedBox: Set[Int],
        allowedInRow: Set[Int],
        allowedInCol: Set[Int]
    ) =
      allowedBox intersect allowedInRow intersect allowedInCol

    def empty: Puzzle = Puzzle(
      Coord.boxCoordinatesCache.flatMap(_._2).map(_ -> Cell.Empty).toMap
    )

    def fromList(cells: List[(Coord, Int)]) =
      cells.foldLeft(empty) { case (p, (coord, value)) =>
        p.update(coord, Given(value))
      }

  }

  object Solver {

    def solve(puzzle: Puzzle): Puzzle = {
      if (puzzle.isSolved) puzzle
      else {
        val filled = fillPossibleValues(puzzle).get // todo
        val eliminated = eliminateSinglePossible(filled)

        if (eliminated == puzzle) {
          println("Stuck: ")
          println(Renderer.render(eliminated))
          throw new Exception("")
        } else solve(eliminated)

      }
    }

    def solveBacktracking(puzzle: Puzzle): IndexedSeq[Puzzle] = {
      println(puzzle.unfilledCount)
      if (puzzle.isSolved) {
        throw new Exception("Solved") 
        IndexedSeq(puzzle)
      }
      else {
        val p = fillPossibleValues(puzzle)
        if (p.isEmpty) IndexedSeq.empty[Puzzle]
        else
          p.get.boxes.flatMap { box =>
            box.possibleCells
              .flatMap { case (coord, pos) =>
                pos.values.map(v => puzzle.update(coord, Cell.Filled(v)))
              }
              .flatMap(solveBacktracking)
          }
      }
    }

    def fillPossibleValues(puzzle: Puzzle): Option[Puzzle] =
      puzzle.boxes.foldLeft(Some(puzzle): Option[Puzzle]) {
        case (Some(puzz), box) =>
          val allowedBoxValues = box.allowedValues
          val cellsWithValues = box.unfilled
            .map { case (coord, _) =>
              coord -> Puzzle.allowedValuesForCell(
                allowedBoxValues,
                puzz.allowedInRowFor(coord),
                puzz.allowedInColFor(coord)
              )
            }

          if (cellsWithValues.exists(_._2.isEmpty)) {
            println("No possible")
            None
          } else {
            val puzzle = cellsWithValues.foldLeft(puzz) {
              case (accPuzzle, (coord, cellValue)) =>
                accPuzzle.update(coord, Cell.Possible(cellValue))
            }
            Some(puzzle)
          }
        case _ => None
      }

    def eliminateSinglePossible(puzzle: Puzzle): Puzzle =
      puzzle.boxes.foldLeft(puzzle) { case (accPuzzle, box) =>
        box.singlePossible.foldLeft(accPuzzle) {
          case (accPuzzle2, (coord, value)) =>
            accPuzzle2.update(coord, Cell.Filled(value))
        }
      }

    def eliminateDoubles(puzzle: Puzzle): Puzzle =
      puzzle

  }

  object Renderer {

    val mRep = 30
    val delim = "-" * mRep + "+" + "-" * mRep + "+" + "-" * mRep

    def render(p: Puzzle): String = {
      (1 to 9)
        .map { row =>
          val renderedRow = (1 to 9)
            .map { col =>
              (p.map(Coord(col, row)) match {
                case Cell.Empty         => "."
                case Cell.Filled(value) => value.toString
                case Cell.Given(value)  => value.toString
                case Cell.Possible(values) =>
                  s"[${values.toList.sorted.mkString("")}]"
              }).padTo(9, ' ')
            }
            .grouped(3)
            .map(_.mkString(" "))
            .mkString(" | ")

          if (row % 3 == 0 && row != 9)
            s"$renderedRow\n$delim"
          else renderedRow
        }
        .mkString("\n")

    }

  }

}
