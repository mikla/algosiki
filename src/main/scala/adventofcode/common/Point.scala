package adventofcode.common

case class Point[T](x: T, y: T)(implicit N: Numeric[T]) {

  def left: Point[T] = copy(x = N.minus(x, N.one))

  def right: Point[T] = copy(x = N.plus(x, N.one))

  def down: Point[T] = copy(y = N.minus(y, N.one))

  def up: Point[T] = copy(y = N.plus(y, N.one))

  def neighbours: List[Point[T]] =
    List(left, right, down, up)

  /*  Manhattan distance https://en.wikipedia.org/wiki/Taxicab_geometry */
  def manh(p: Point[T]): T =
    N.abs(N.plus(N.minus(x, p.x), N.abs(N.minus(y, p.y))))
}

object Point {

  type PointI = Point[Int]
  type PointL = Point[Long]

}
