package com.rothko.fightboats

import scala.collection.mutable
import scala.util.Random

sealed trait StrikeResult
case object Miss extends StrikeResult
case object HitAndSunk extends StrikeResult
case object Hit extends StrikeResult

case class Board(boats: List[Boat], strikes: List[Coordinate]) {
  import Board._

  lazy val allCoveringPosition: List[Coordinate] = boats.flatMap(_.coveringPositions)

  lazy val allShipsDestroyed: Boolean = allCoveringPosition.forall(strikes.contains)

  lazy val hits: List[Coordinate] = strikes.filter(allCoveringPosition.contains)

  lazy val misses: List[Coordinate] = strikes.filterNot(allCoveringPosition.contains)

  def knownHit(c: Coordinate): Boolean = hits.contains(c)

  def knownMiss(c: Coordinate): Boolean = misses.contains(c)

  def strike(coordinate: Coordinate): (StrikeResult, Board) = {
    val newStrikes = coordinate :: strikes
    val hitBoat = boats.find(_.covers(coordinate))
    val result = hitBoat match {
      case Some(b) if b.coveringPositions.forall(newStrikes.contains) => HitAndSunk
      case Some(_) => Hit
      case None => Miss
    }
    (result, copy(strikes = newStrikes))
  }

  lazy val notStruck: List[Coordinate] = Board.allSquares.filterNot(strikes.contains)
}

object Board {
  val height = 10
  val width = 10

  def contains(coordinate: Coordinate): Boolean = 0 <= coordinate.x && coordinate.x < height && 0 <= coordinate.y && coordinate.y < width

  def contains(boat: Boat): Boolean = boat.coveringPositions.forall(contains)

  def allInValidPositions(boats: List[Boat]): Boolean = {
    val positions = boats.flatMap(_.coveringPositions)
    lazy val boatsWithinBoards = positions.forall(contains)
    lazy val noOverlaps = positions.distinct.length == positions.length

    boatsWithinBoards && noOverlaps
  }

  def create(boats: List[Boat]): Board = {
    require(allInValidPositions(boats))
    Board(boats, Nil)
  }

  def generateBoard: Board = generateBoards(Random).head

  def generateBoards(sample: Int) : List[Board] = generateBoards(Random).take(sample).toList

  private def generateBoards(random: Random): LazyList[Board] = {
    val games = for {
      sub1 <- generateAllPositions(random, Submarine.apply)
      sub2 <- generateAllPositions(random, Submarine.apply)
      dest1 <- generateAllPositions(random, Destroyer.apply)
      dest2 <- generateAllPositions(random, Destroyer.apply)
      cruiser <- generateAllPositions(random, Cruiser.apply)
      battleship <- generateAllPositions(random, Battleship.apply)
      carrier <- generateAllPositions(random, Carrier.apply)
    } yield {
      List(sub1, sub2, dest1, dest2, cruiser, battleship, carrier)
    }
    games.filter(allInValidPositions).map(Board(_, Nil))
  }

  private def generateAllPositions[T <: Boat](random: Random, fn: (Coordinate, Orientation) => T): LazyList[T] = {
    val coords = for {
      x <- random.shuffle((0 until width).toList)
      y <- random.shuffle((0 until height).toList)
      orientation <- random.shuffle(List(Vertical, Horizontal))
    } yield {
      (Coordinate(x, y), orientation)
    }

    LazyList(coords :_*).map { case (c, o) => fn(c, o) }.filter(contains)
  }

  val allSquares: List[Coordinate] = {
    (for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      Coordinate(x, y)
    }).toList
  }

  private def asStringHelper(repFn: Coordinate => String)(board: Board) = {
    Board.allSquares.foldRight(new mutable.StringBuilder())((c, builder) => {
      if c.y == height - 1 then builder.append("\n") else ()
      builder.append(" ").append(repFn(c)).append(" ")
    }).toString
  }

  def asStringForAttacker(board: Board): String = asStringHelper {
    c =>
      if board.knownHit(c) then "X"
      else if board.knownMiss(c) then "?"
      else "-"
  }(board)

  def asStringForDefender(board: Board): String = asStringHelper {
    c =>
      if board.knownHit(c) then "X"
      else if board.knownMiss(c) then "?"
      else if board.allCoveringPosition.contains(c) then "O"
      else "-"
  }(board)
}
