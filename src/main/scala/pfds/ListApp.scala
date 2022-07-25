package pfds

object ListApp extends App {

  def update[A](l: List[A], i: Int, y: A): List[A] =
    (l, i) match {
      case (x :: xs, 0) => y :: l.tail
      case (x :: xs, i) =>
        x :: update(xs, i - 1, y)
    }

  def suffixes[A](l: List[A]): List[List[A]] = {

    def loop(acc: List[List[A]], rest: List[A]): List[List[A]] =
      rest match {
        case x :: xs =>
          loop((x :: xs) :: acc, xs)
        case Nil => Nil :: acc
      }

    loop(List.empty[List[A]], l).reverse
  }

}
