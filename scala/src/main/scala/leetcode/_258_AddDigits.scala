package leetcode

object _258_AddDigits extends App {

  def addDigits(num: Int): Int = {
    var x = num
    while (x > 9) {
      x = x.toString.map(_.asDigit).sum
    }
    x
  }

}
