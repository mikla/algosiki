package alg

object CoinChange extends App {

  def coinChange(amount: Int, coins: List[Int]): Map[Int, Int] = {

    def loop(
        amountLeft: Int,
        coinsLeftSorted: List[Int],
        change: Map[Int, Int]
    ): Map[Int, Int] =
      if (amountLeft == 0) change
      else
        coinsLeftSorted match {
          case coin :: rest =>
            loop(amountLeft % coin, rest, change + (coin -> amountLeft / coin))
          case Nil => sys.error("No posible")
        }

    loop(amount, coins.sorted.reverse, Map.empty)
  }

  println(coinChange(33, List(1, 5, 10, 25))) // Map(25 -> 1, 10 -> 0, 5 -> 1, 1 -> 3)

}
