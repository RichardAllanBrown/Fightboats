package com.rothko.fightboats

class StrategyRunnerSuite extends munit.FunSuite {
  test("Random will run to the end for a single boat") {
    val board = Board.create(List(Cruiser(Coordinate(1, 1), Vertical)))
    val states = StrategyRunner.applyStrategy(RandomStrategy, board)
    assert(states.head.allShipsDestroyed)
  }

  test("Cheater will take perfect number of turns") {
    val boat = Cruiser(Coordinate(1, 1), Vertical)
    val board = Board.create(List(boat))
    val states = StrategyRunner.applyStrategy(PerfectStrategy, board)
    assertEquals(boat.length, states.length)
  }

  test("Cheater takes perfect number of turns for standard board") {
    val board = Board.generateBoard
    val states = StrategyRunner.applyStrategy(PerfectStrategy, board)
    assertEquals(board.boats.map(_.length).sum, states.length)
  }

  test("Cheater takes perfect number of turns for all boards") {
    val res = StrategyRunner.evaluateStrategy(PerfectStrategy, 10000)
    assertEquals(18, res)
  }

  test("Worst strategy takes maximum number of turns for all boards") {
    val res = StrategyRunner.evaluateStrategy(WorstStrategy, 10)
    assertEquals(Board.width * Board.height, res)
  }

  test("HuntAndDestroy can solve a board") {
    val initialBoard = Board.generateBoard
    println(Board.asStringForDefender(initialBoard))
    println("Applying HuntAndDestroy strategy")
    val res = StrategyRunner.applyStrategy(new HuntAndDestroy(), initialBoard)
    println(res.head.strikes.reverse)
    println(Board.asStringForAttacker(res.head))
    assert(res.head.allShipsDestroyed)
  }
}
