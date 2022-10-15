package com.rothko.fightboats

sealed trait CompassPoint {
  def translateBy(distance: Int)(source: Coordinate): Coordinate
}
case object North extends CompassPoint {
  def translateBy(distance: Int)(source: Coordinate): Coordinate = source.copy(x = source.x - distance)
}
case object East extends CompassPoint {
  def translateBy(distance: Int)(source: Coordinate): Coordinate = source.copy(y = source.y + distance)
}
case object South extends CompassPoint {
  def translateBy(distance: Int)(source: Coordinate): Coordinate = source.copy(x = source.x + distance)
}
case object West extends CompassPoint {
  def translateBy(distance: Int)(source: Coordinate): Coordinate = source.copy(y = source.y - distance)
}
