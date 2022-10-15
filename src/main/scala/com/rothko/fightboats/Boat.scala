package com.rothko.fightboats

sealed trait Boat {
  def position: Coordinate
  def orientation: Orientation
  def length: Int

  def coveringPositions: Seq[Coordinate] = orientation match {
    case Horizontal => (0 until length).map(d => position.copy(x = position.x + d))
    case Vertical => (0 until length).map(d => position.copy(y = position.y + d))
  }

  def covers(c: Coordinate): Boolean = coveringPositions.contains(c)
}
case class Submarine(position: Coordinate, orientation: Orientation) extends Boat {
  override def length = 1
}
case class Destroyer(position: Coordinate, orientation: Orientation) extends Boat {
  override def length = 2
}
case class Cruiser(position: Coordinate, orientation: Orientation) extends Boat {
  override def length = 3
}
case class Battleship(position: Coordinate, orientation: Orientation) extends Boat {
  override def length = 4
}
case class Carrier(position: Coordinate, orientation: Orientation) extends Boat {
  override def length = 5
}
