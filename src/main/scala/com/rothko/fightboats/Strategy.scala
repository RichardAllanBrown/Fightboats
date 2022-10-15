package com.rothko.fightboats

import scala.util.Random

trait Strategy {
  def nextTarget(board: Board, previousResult: Option[StrikeResult]): Coordinate
}

object RandomStrategy extends Strategy {
  override def nextTarget(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
    board.notStruck(Random.nextInt(board.notStruck.length))
  }
}

object PerfectStrategy extends Strategy {
  override def nextTarget(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
    board.boats.flatMap(_.coveringPositions).filterNot(board.strikes.contains).head
  }
}

object WorstStrategy extends Strategy {
  override def nextTarget(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
    val (covering, notCovering) = board.notStruck.partition(board.allCoveringPosition.contains)
    (notCovering ::: covering).head
  }
}

// Scan via a checkerboard, if found boat, fire vertical and horizontal until destoyed, then return to hunt
class HuntAndDestroy extends Strategy {
  trait Mode {
    def identifyNextCoordinate(board: Board, previousResult: Option[StrikeResult]): Coordinate
  }
  case object HuntMode extends Mode {
    def identifyNextCoordinate(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
      val (blackSquares, whiteSquares) = board.notStruck.partition(c => (c.x + c.y) % 2 == 0)
      (blackSquares ::: whiteSquares).head
    }
  }
  case class DestroyMode(from: Coordinate) extends Mode {
    def identifyNextCoordinate(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
      List(North, East, South, West).flatMap(direction => {
        (1 to 5).map(distance => direction.translateBy(distance)(from))
          .filter(Board.contains)
          .dropWhile(board.knownHit)
          .takeWhile(c => !board.knownMiss(c))
      }).head
    }
  }

  var mode: Mode = HuntMode

  override def nextTarget(board: Board, previousResult: Option[StrikeResult]): Coordinate = {
    mode = (mode, previousResult) match {
      case (_, Some(HitAndSunk)) => HuntMode
      case (HuntMode, Some(Hit)) => DestroyMode(board.strikes.head)
      case _ => mode
    }

    mode.identifyNextCoordinate(board, previousResult)
  }
}
