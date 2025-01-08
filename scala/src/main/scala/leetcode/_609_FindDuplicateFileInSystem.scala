package leetcode

object _609_FindDuplicateFileInSystem extends App {

  object Solution {

    case class File(path: String, content: String)

    def findDuplicate(paths: Array[String]): List[List[String]] =
      paths
        .map(_.split(" ").toList)
        .flatMap { parts =>
          val path = parts.head
          val filesAndContent = parts.tail
          filesAndContent.map { case s"$fileName($content)" =>
            File(path + "/" + fileName, content)
          }
        }
        .groupBy(_.content)
        .view
        .values
        .filter(_.length > 1)
        .toList
        .map(_.map(_.path).toList)

  }

  Solution.findDuplicate(
    Array(
      "root/a 1.txt(abcd) 2.txt(efgh)",
      "root/c 3.txt(abcd)",
      "root/c/d 4.txt(efgh)",
      "root 4.txt(efgh)"
    )
  )

  println {
    Solution.findDuplicate(
      Array(
        "root/a 1.txt(abcd) 2.txt(efsfgh)",
        "root/c 3.txt(abdfcd)",
        "root/c/d 4.txt(efggdfh)"
      )
    )
  }

}
