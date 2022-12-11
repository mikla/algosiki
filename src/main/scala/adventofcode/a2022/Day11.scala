package adventofcode.a2022

import adventofcode.a2022.Day11InputData._

object Day11 extends App {

  type State = Map[Long, Monkey]

  case class Monkey(
      id: Long,
      items: List[BigInt],
      operation: BigInt => BigInt,
      divisibleBy: BigInt,
      ifTrue: Long,
      ifFalse: Long
  ) {
    def inspected: Monkey = copy(items = Nil)
    def catchItem(item: BigInt): Monkey = copy(items = items.appended(item))
  }

  case class Throw(item: BigInt, to: Long)

  case class Inspected(itemsCount: BigInt, actions: List[Throw])

  case class Stats(map: Map[Long, BigInt]) {
    def update(monkeyId: Long, inspected: BigInt): Stats = copy(
      map.updatedWith(monkeyId)(_.map(_ + inspected))
    )
  }

  object Stats {
    def init(monkeysCount: Int): Stats = Stats(
      (0 until monkeysCount).map(x => (x.toLong, BigInt(0))).toMap
    )
  }

  def inspect(monkey: Monkey): Inspected = {
    val actions = monkey.items.map { item =>
      val worried = monkey.operation(item)
//      val bored = Math.floor(worried / 3d).toLong // part 1
      val bored =
        worried % 9699690 // part2: product of all multipliers (divisibleBy)
                          //
      if (bored % monkey.divisibleBy == 0)
        Throw(bored, monkey.ifTrue)
      else Throw(bored, monkey.ifFalse)
    }

    Inspected(monkey.items.size, actions)
  }

  def updateState(
      state: State,
      stats: Stats,
      monkey: Monkey,
      inspected: Inspected
  ): (State, Stats) = {

    val newState = inspected.actions
      .foldLeft(state) { case (acc, t) =>
        acc.updatedWith(t.to)(_.map(_.catchItem(t.item)))
      }
      .updatedWith(monkey.id)(_.map(_.inspected))

    (newState, stats.update(monkey.id, inspected.itemsCount))
  }

  def round(state: State, stats: Stats, monkey: Monkey): (State, Stats) =
    updateState(state, stats, monkey, inspect(monkey))

  lazy val InputData: List[Monkey] = Test1
  lazy val MonkeysTotal = InputData.size
  lazy val RoundsTotal = 10000 * MonkeysTotal

  lazy val state: State =
    InputData.groupBy(_.id).view.mapValues(_.head).toMap

  def simulateRounds(count: Long): (State, Stats) =
    (0L until count)
      .map(_ % MonkeysTotal)
      .foldLeft((state, Stats.init(MonkeysTotal))) {
        case ((state, stats), monkeyId) =>
          round(state, stats, state(monkeyId))
      }

  val simulated = simulateRounds(RoundsTotal)

  val part1 =
    simulated._2.map.values.toList.sorted.reverse
      .take(2)
      .product

  println(part1)

}
