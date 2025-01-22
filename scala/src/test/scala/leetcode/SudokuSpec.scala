package leetcode

import leetcode._37_SudokuSolver.{Cell, Coord, Puzzle, Renderer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import leetcode._37_SudokuSolver.Solver

class SudokuSpec extends AnyFlatSpec with should.Matchers {

  "Coord.box(1)" should "give correct box by index" in {
    Coord.box(1) shouldBe Set(
      Coord(1, 1),
      Coord(1, 2),
      Coord(1, 3),
      Coord(2, 1),
      Coord(2, 2),
      Coord(2, 3),
      Coord(3, 1),
      Coord(3, 2),
      Coord(3, 3)
    )
  }

  "Coord.box(6)" should "give correct coords" in {
    Coord.box(6) shouldBe Set(
      Coord(7, 4),
      Coord(8, 4),
      Coord(9, 4),
      Coord(7, 5),
      Coord(8, 5),
      Coord(9, 5),
      Coord(7, 6),
      Coord(8, 6),
      Coord(9, 6)
    )
  }

  "Puzzle.empty" should "give up empty Puzzle map" in {
    Puzzle.empty.map.values.size shouldBe 81
    for {
      x <- (1 to 9).toList
      y <- (1 to 9).toList
    } yield Puzzle.empty.map(Coord(x, y)) shouldBe Cell.Empty
  }

  "Renderer" should "render" in {
    println(Renderer.render(testPuzzle))
  }

  "Puzzle.allowedInRowFor" should "give possible allowed values having only row" in {
    testPuzzle.allowedInRowFor(Coord(3, 1)) shouldBe Set(1, 2, 6, 7)
  }

  "Puzzle.allowedInColFor" should "should return col values allowed" in {
    testPuzzle.allowedInColFor(Coord(3, 1)) shouldBe Set(1, 2, 3, 4, 5, 6, 9)
  }

  "fillPossibleValues" should "fill possible values" in {
    val puzzle1 = Solver.fillPossibleValues(testPuzzle)
  
    puzzle1.get.map(Coord(3, 1)) shouldBe Cell.Possible(Set(1, 2, 6))
  }

  "fillPossibleValues" should "return none if impossible to find possible values" in {
    Solver.fillPossibleValues(testPuzzle.update(Coord(3,1), Cell.Filled(8))) shouldBe None
  }

  // "Puzzle.solve" should "try to solve puzzle" ignore {
    // Solver.solve(testPuzzle)
  // }

  "Puzzle.solveBacktracking" should "solve backtracking" in {
    println(Solver.solveBacktracking(testPuzzle))
  }

  val testPuzzle = Puzzle.fromList(
    List(
      Coord(1, 1) -> 8,
      Coord(2, 1) -> 4,
      Coord(1, 2) -> 9,
      Coord(3, 3) -> 7,
      Coord(4, 1) -> 3,
      Coord(5, 1) -> 9,
      Coord(5, 2) -> 8,
      Coord(4, 3) -> 2,
      Coord(8, 1) -> 5,
      Coord(7, 2) -> 4,
      Coord(2, 5) -> 5,
      Coord(4, 4) -> 9,
      Coord(5, 4) -> 5,
      Coord(4, 5) -> 7,
      Coord(5, 5) -> 3,
      Coord(6, 5) -> 2,
      Coord(5, 6) -> 1,
      Coord(6, 6) -> 4,
      Coord(8, 5) -> 6,
      Coord(3, 8) -> 8,
      Coord(2, 9) -> 9,
      Coord(6, 7) -> 9,
      Coord(5, 8) -> 6,
      Coord(5, 9) -> 2,
      Coord(6, 9) -> 3,
      Coord(7, 7) -> 1,
      Coord(9, 8) -> 9,
      Coord(8, 9) -> 8,
      Coord(9, 9) -> 5
    )
  )

}
