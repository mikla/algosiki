package leetcode

// https://leetcode.com/problems/utf-8-validation/
object _393_UTF8Validation extends App {

  type BinaryString = String

  def toBinary(n: Int): BinaryString =
    n match {
      case 0 | 1 => s"$n"
      case _     => s"${toBinary(n / 2)}${n % 2}"
    }

  def toBinaryPadded(n: Int) = toBinary(n).reverse.padTo(8, '0').reverse

  def length(firstByteString: BinaryString): Int = {
    val check: BinaryString => Boolean = firstByteString.startsWith

    if (check("0")) 1
    else if (check("110")) 2
    else if (check("1110")) 3
    else if (check("11110")) 4
    else 0
  }

  val isFollowed: BinaryString => Boolean = _.startsWith("10")

  def isValidSeq(chars: List[BinaryString]): Boolean = chars match {
    case one :: Nil =>
      one.startsWith("0")
    case one :: two :: Nil =>
      one.startsWith("110") && isFollowed(two)
    case one :: two :: three :: Nil =>
      one.startsWith("1110") && isFollowed(two) && isFollowed(three)
    case one :: two :: three :: four :: Nil =>
      one.startsWith("11110") && isFollowed(two) && isFollowed(
        three
      ) && isFollowed(four)
    case Nil => false
    case _   => false
  }

  def validUtf8(data: Array[Int]): Boolean = {

    def loop(
        data: List[BinaryString],
        acc: Option[List[List[String]]]
    ): Option[List[List[String]]] =
      if (data.isEmpty || acc.isEmpty) acc
      else {
        val len = length(data.head)
        if (len > 0)
          loop(data.drop(len), acc.map(data.take(len) :: _))
        else None
      }

    loop(
      data.map(toBinaryPadded).toList,
      Some(List.empty[List[String]])
    ) match {
      case Some(value) =>
        value.map(isValidSeq).forall(_ == true)
      case None => false

    }
  }
}
