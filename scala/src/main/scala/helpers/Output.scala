package helpers

object Output {

  def print2DArray[T](array: Array[Array[T]]): Unit =
    array.foreach { row =>
      println("")
      row.foreach(print(_) + " ")
    }

}
