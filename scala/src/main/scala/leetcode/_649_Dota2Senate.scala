package leetcode

object _649_Dota2Senate extends App {

  import scala.collection.immutable.Queue
  def predictPartyVictory(senate: String): String = {

    val len = senate.length

    val (rQ, dQ) =
      senate.zipWithIndex.foldLeft((Queue.empty[Int], Queue.empty[Int])) {
        case ((rQueue, dQueue), (c, index)) =>
          c match {
            case 'R' => (rQueue.enqueue(index), dQueue)
            case _   => (rQueue, dQueue.enqueue(index))
          }
      }

    def loop(rQueue: Queue[Int], dQueue: Queue[Int]): String = {
      if (rQueue.nonEmpty && dQueue.nonEmpty) {
        val (rTurn, rTurnQ) = rQueue.dequeue
        val (dTurn, dTurnQ) = dQueue.dequeue
        if (dTurn < rTurn) loop(rTurnQ, dTurnQ.enqueue(dTurn + len))
        else loop(rTurnQ.enqueue(rTurn + len), dTurnQ)
      } else if (rQueue.isEmpty) "Dire"
      else "Radiant"
    }

    loop(rQ, dQ)

  }


  println(predictPartyVictory("RDD"))
  println(predictPartyVictory("RD"))

}
