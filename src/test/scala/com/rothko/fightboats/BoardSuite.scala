package com.rothko.fightboats

class BoardSuite extends munit.FunSuite {
  test("All boats start as intact") {
    val board = Board.create(List(Submarine(Coordinate(0, 0), Vertical)))
    assertEquals(false, board.allShipsDestroyed)
  }
  test("Will tell us of a miss") {
    val board = Board.create(Nil)
    val (res, _) = board.strike(Coordinate(0, 0))
    assert(Miss == res)
  }
  test("Will tell us of a hit that leads to a sink") {
    val board = Board.create(List(Submarine(Coordinate(0, 0), Vertical)))
    val (res, _) = board.strike(Coordinate(0, 0))
    assert(HitAndSunk == res)
  }
  test("Will tell us of a hit that isn't a sink") {
    val board = Board.create(List(Cruiser(Coordinate(0, 0), Vertical)))
    val (res, _) = board.strike(Coordinate(0, 0))
    assert(Hit == res)
  }
  test("Strikes are added to set of strikes") {
    val board = Board.create(Nil)
    val (_, newBoard) = board.strike(Coordinate(0, 0))
    assert(newBoard.strikes.contains(Coordinate(0, 0)))
  }
  test("All ships destroyed when all are hit") {
    val board = Board.create(List(Submarine(Coordinate(0, 0), Vertical)))
    val (_, newBoard) = board.strike(Coordinate(0, 0))
    assert(newBoard.allShipsDestroyed)
  }
  test("Can generate a random board") {
    val board = Board.generateBoard
    assertEquals(7, board.boats.length)
  }
}
