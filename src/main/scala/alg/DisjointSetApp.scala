package alg

// Attempt to have functional implementation of Union find
object DisjointSetApp extends App {

  trait IDisjointSet[T, F[_]] {

    def makeSet(a: T): F[T]

    def find(a: T): Option[(T, Int)]

    def union(a: T, b: T)(implicit
        O: Ordering[T]
    ): Option[(Boolean, F[T])]

  }

  case class DisjointSet[T](backed: Map[T, (T, Int)])
      extends IDisjointSet[T, DisjointSet] {

    override def makeSet(a: T): DisjointSet[T] = DisjointSet(
      backed.updated(a, (a, 1))
    )

    override def find(a: T): Option[(T, Int)] =
      backed.get(a) match {
        case p @ Some((v, nums)) =>
          if (v == a) p
          else find(v)
        case _ => None
      }

    override def union(a: T, b: T)(implicit
        O: Ordering[T]
    ): Option[(Boolean, DisjointSet[T])] = {
      for {
        ap @ (ar, _) <- find(a)
        bp @ (br, _) <- find(b)
      } yield {
        def nd(ap: (T, Int), bp: (T, Int)) = {
          val rmax = O.max(ap._1, bp._1)
          val maxn = Math.max(ap._2, bp._2)

          val rmin = O.min(ap._1, bp._1)
          val minn = Math.min(ap._2, bp._2)

          val nm = backed.updated(rmax, (rmax, maxn + minn))
          DisjointSet(nm.updated(rmin, (rmax, minn)))
        }

        if (ar == br) (false, this) else (true, nd(ap, bp))
      }
    }
  }

  object DisjointSet {

    def empty[T]: DisjointSet[T] = DisjointSet(Map.empty)

    def apply[T](): DisjointSet[T] = empty

    def fromList[T](l: List[T]): DisjointSet[T] = {
      l.foldLeft(DisjointSet[T]()) { case (acc, elem) =>
        acc.makeSet(elem)
      }
    }

    def dunion[T](a: T, b: T, ds: DisjointSet[T])(implicit
        O: Ordering[T]
    ): Option[DisjointSet[T]] =
      ds.union(a, b).map(_._2)

  }

}
