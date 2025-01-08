package adventofcode.a2020

import helpers.Input
import monix.eval.Task

import scala.concurrent.duration.Duration

object Day20JurassicJigsaw extends App {

  case class Tile(id: Int, bitmap: Array[Array[Char]]) {

    override def toString: String = {
      s"Tile $id" + "\n" + bitmap.map(_.mkString).mkString("\n")
    }

    val N: Int = bitmap.length

    private def transform(reMap: (Int, Int) => Unit): Unit = {
      var i = 0
      while (i < N) {
        var j = 0
        while (j < N) {
          reMap(i, j)
          j = j + 1
        }
        i = i + 1
      }
    }

    def rotate90Clockwise: Tile = {
      val newBimap = Array.ofDim[Char](bitmap.length, bitmap.length)
      transform((i, j) => newBimap(j)(N - 1 - i) = bitmap(i)(j))
      copy(bitmap = newBimap)
    }

    def flipVertical: Tile = {
      val newBimap = Array.ofDim[Char](bitmap.length, bitmap.length)
      transform((i, j) => newBimap(N - 1 - i)(j) = bitmap(i)(j))
      copy(bitmap = newBimap)
    }

    def flipHorizontal: Tile = {
      val newBimap = Array.ofDim[Char](bitmap.length, bitmap.length)
      transform((i, j) => newBimap(i)(N - 1 - j) = bitmap(i)(j))
      copy(bitmap = newBimap)
    }

    def topRow: List[Char] = (0 until N).map(j => bitmap(0)(j)).toList
    def rightCol: List[Char] = (0 until N).map(j => bitmap(j)(N - 1)).toList
    def bottomRow: List[Char] = (0 until N).map(j => bitmap(N - 1)(j)).toList
    def leftCol: List[Char] = (0 until N).map(j => bitmap(j)(0)).toList

  }

  case class Image(tiles: Array[Array[Option[Tile]]]) {
    val N: Int = tiles.length

    def id: Long = List(
      tiles(0)(0),
      tiles(0)(N - 1),
      tiles(N - 1)(0),
      tiles(N - 1)(N - 1)
    ).flatten.map(_.id.toLong).product

    def isCoordsOk(i: Int, j: Int): Boolean =
      i >= 0 && i < N && j >= 0 && j < N

    def getTile(i: Int, j: Int): Option[Tile] =
      if (isCoordsOk(i, j)) tiles(i)(j) else None

    // no rotation here
    def fits(i: Int, j: Int, tile: Tile): Boolean =
      getTile(i, j - 1).forall(_.rightCol == tile.leftCol) &&
        getTile(i, j + 1).forall(_.leftCol == tile.rightCol) &&
        getTile(i - 1, j).forall(_.bottomRow == tile.topRow) &&
        getTile(i + 1, j).forall(_.topRow == tile.bottomRow) &&
        List(
          getTile(i, j - 1),
          getTile(i, j + 1),
          getTile(i - 1, j),
          getTile(i + 1, j)
        ).exists(_.isDefined)

    // rotate 3 time + flip vertical and horizontal
    def fitsToWithRotation(tile: Tile): Option[((Int, Int), Tile)] = {
      def meta(fits: Boolean, x: Int, y: Int, tile: Tile) = (fits, x, y, tile)

      (for {
        i <- 0 until N
        j <- 0 until N
      } yield List(
        tile,
        tile.flipVertical,
        tile.flipHorizontal,
        tile.rotate90Clockwise,
        tile.rotate90Clockwise.rotate90Clockwise,
        tile.rotate90Clockwise.rotate90Clockwise.rotate90Clockwise
      ).map(t => meta(fits(i, j, t), i, j, t))).toList.flatten.collectFirst {
        case (true, i, j, t) => ((i, j), t)
      }
    }

    def placeTile(i: Int, j: Int, tile: Tile): Image = {
      val newTiles = tiles.transpose.transpose
      newTiles(i)(j) = Some(tile)
      copy(tiles = newTiles)
    }

    override def toString: String =
      tiles.map(_.map(_.map(_.id).toString).mkString).mkString("\n")
  }

  object Image {
    def initWith(width: Int, tile: Tile): Image = {
      val tilesArr = Array.fill(width, width)(None: Option[Tile])
      tilesArr(0)(0) = Some(tile)
      Image(tilesArr)
    }
  }

  val lines =
    Input.readListString("src/main/scala/a2020/Day20JurassicJigsaw.in") :+ ""

  val tiles = lines
    .foldLeft((List.empty[Tile], Array.empty[Array[Char]], 0)) {
      case ((allTiles, currentTile, tileId), inputData) =>
        inputData match {
          case s"Tile $id:" => (allTiles, currentTile, id.toInt)
          case "" =>
            (Tile(tileId, currentTile) :: allTiles, Array.empty[Array[Char]], 0)
          case bitMap => (allTiles, currentTile :+ bitMap.toCharArray, tileId)
        }
    }
    ._1

  def reassembleImage(img: Image, tiles: List[Tile]): Option[Image] = {
    tiles match {
      case Nil =>
        println(img.id)
        Some(img)
      case tilesLeft =>
        tilesLeft.flatMap { tile =>
          val coordsFit = img.fitsToWithRotation(tile)
          if (coordsFit.isDefined) {
            val ((x, y), fittedTile) = coordsFit.get
            reassembleImage(
              img.placeTile(x, y, fittedTile),
              tiles.filterNot(_.id == fittedTile.id)
            )
          } else None
        }.headOption
    }
  }

  //  val img = tiles.foldLeft(None: Option[Image]) { case (acc, tile) =>
  //    acc match {
  //      case None => Task(reassembleImage(Image.initWith(Math.sqrt(tiles.length).toInt, tile), tiles.filterNot(_.id == tile.id)))
  //      case img  => img
  //    }
  //  }.map(_.id)

  val img = {

    val res = Task
      .parSequenceN(12)(
        tiles.map(tile =>
          Task(
            reassembleImage(
              Image.initWith(Math.sqrt(tiles.length).toInt, tile),
              tiles.filterNot(_.id == tile.id)
            )
          )
        )
      )
      .runSyncUnsafe(Duration.Inf)(monix.execution.Scheduler.global, implicitly)
      .find(_.isDefined)
      .flatten
      .map(_.id)
  }

  println(img)

  val Tile1951 = tiles.find(_.id == 1951)
  val Tile2311 = tiles.find(_.id == 2311)

  //  println(Tile1951.get.flipVertical)

  val image = Image(
    Array(
      Array(Tile1951.map(_.flipVertical), None, None),
      Array(None, None, None),
      Array(None, None, None)
    )
  )

}
