package leetcode

object _290_WordPattern extends App {

  def wordPattern(pattern: String, s: String): Boolean = {
    val words = s.split(" ")

    if (words.length != pattern.length) false
    else {
      pattern
        .zip(words)
        .foldLeft((Map.empty[Char, String], true)) {
          case ((map, true), (varr, value)) =>
            map.find(_._2 == value) match {
              case Some((key, v)) =>
                if (key == varr) (map, true) else (map, false)
              case None =>
                map.get(varr) match {
                  case Some(v1) =>
                    if (v1 == value) (map, true) else (map, false)
                  case None =>
                    (map + (varr -> value), true)
                }

            }
          case ((map, false), (_, _)) => (map, false)
        }
        ._2
    }

  }

  println(wordPattern("abba", "dog cat cat dog"))
  println(wordPattern("abba", "dog cat cat fish"))
  println(wordPattern("abba", "dog dog dog dog"))
  println(wordPattern("aaaa", "dog cat cat dog"))
  println(wordPattern("a", "a"))
  println(wordPattern("abc", "dog cat dog"))

}
