package pfds.chapter2

import org.scalatest.flatspec._
import org.scalatest.matchers._
import pfds.chapter2.ListApp.{suffixes, update}

class ListAppSpec extends AnyFlatSpec with should.Matchers {

  "A list.update" should "update element by index" in {
    update(List(1, 2, 3, 4, 5), 2, -1) should contain theSameElementsAs (List(
      1, 2, -1, 4, 5
    ))
  }

  "a suffixes" should "return list of suffixes" in {
    suffixes(List(1, 2, 3, 4)) should contain theSameElementsAs List(
      List(1, 2, 3, 4),
      List(2, 3, 4),
      List(3, 4),
      List(4),
      List()
    )
  }

}
