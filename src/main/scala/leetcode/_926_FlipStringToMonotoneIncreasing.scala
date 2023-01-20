package leetcode

object _926_FlipStringToMonotoneIncreasing extends App {

  def minFlipsMonoIncr(s: String): Int = {
    s.foldLeft((0, 0)) { case ((one, fl), ch) =>
      if (ch == '1') (one + 1, fl)
      else (one, Math.min(fl + 1, one))
    }._2
  }

}
