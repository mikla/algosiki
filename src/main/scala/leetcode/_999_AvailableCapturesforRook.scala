package leetcode

object _999_AvailableCapturesforRook extends App {

  type Coord = (Int, Int)

  def numRookCaptures(board: Array[Array[Char]]): Int = {
    val (bishop, rook, pawn, empty) = ('B', 'R', 'p', '.')

    val findRook = board.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.collectFirst { case (ch, j) if ch == rook => (i, j) }
    }.head

    def moveToUntil(bishop: Coord, move: Coord => Coord) = ???

    0

  }

}
