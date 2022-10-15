package com.rothko.fightboats

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object StrategyRunner {
  def applyStrategy(strategy: Strategy, board: Board): List[Board] = {

    @tailrec
    def applyStrategyRecHelper(b: Board, prevResult: Option[StrikeResult], collector: List[Board]): List[Board] = {
      val target = strategy.nextTarget(b, prevResult)
      val (result, newBoard) = b.strike(target)
      if (newBoard.allShipsDestroyed) newBoard :: collector
      else applyStrategyRecHelper(newBoard, Some(result), newBoard :: collector)
    }

    applyStrategyRecHelper(board, None, Nil)
  }

  def countTurns(strategy: Strategy, board: Board): Int = {
    applyStrategy(strategy, board).length
  }

  def evaluateStrategy(strategy: Strategy, sample: Int): Int = {
    val boards = Board.generateBoards(sample).toList
    println(s"Generated ${boards.length} boards")
    val hits = boards.map(countTurns(strategy, _))
    hits.sum / hits.length
  }
}
