package leetcode

object _1996_TheNumberOfWeakCharactersInTheGame extends App {

  object Solution {

    def numberOfWeakCharacters(properties: Array[Array[Int]]): Int = {

      val sortedMap: Map[Int, Array[Array[Int]]] =
        properties
          .sortBy { case Array(k, _) => k }
          .groupBy { case Array(at, _) =>
            at
          }

      sortedMap.view.keys.toList.sorted.map { key =>
        sortedMap(key).foldLeft(0) { case (acc, c) =>
          if (isWeak(c, charactersWithGreaterAttack(key, sortedMap))) acc + 1
          else acc
        }
      }.sum

    }

    def isWeak(
        ch: Array[Int],
        allCharactersToAsses: Iterable[Array[Int]]
    ): Boolean =
      allCharactersToAsses.exists { case Array(_, de) =>
        de > ch(1)
      }

    def charactersWithGreaterAttack(
        attack: Int,
        characters: Map[Int, Array[Array[Int]]]
    ): Iterable[Array[Int]] =
      characters.view.filterKeys(_ > attack).values.flatten
  }

}
