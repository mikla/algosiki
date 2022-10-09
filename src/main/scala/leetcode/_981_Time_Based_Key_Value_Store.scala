package leetcode

import scala.collection._

object _981_Time_Based_Key_Value_Store extends App {

  class TimeMap() {

    type KVStore = immutable.HashMap[String, String]

    type TimeStore = mutable.TreeMap[Int, KVStore]

    val map: TimeStore = mutable.TreeMap[Int, KVStore]()

    def set(key: String, value: String, timestamp: Int) {
      val queriedMap =
        map.getOrElseUpdate(timestamp, {
          map.maxBefore(timestamp).map(_._2).getOrElse(immutable.HashMap.empty)
        })
      map += timestamp -> (queriedMap + (key -> value))
    }

    def get(key: String, timestamp: Int): String = {
      val queriedMap = map.getOrElse[KVStore](timestamp,
        map.maxBefore(timestamp).map(_._2).getOrElse(immutable.HashMap.empty)
      )
      queriedMap.getOrElse(key, "")
    }

  }

}
