package models

import scala.math.abs

case class Vector(x: Int, y: Int) {

  override def toString: String = s"$x,$y"
  def toDirection: String = (x, y) match {
    case (1, 0) => "RIGHT"
    case (0, 1) => "UP"
    case (-1, 0) => "LEFT"
    case (0, -1) => "DOWN"
    case _ => "INVALID"
  }

  def distance: Int = abs(x) + abs(y)
  def rotateCCW: Vector = Vector(-y, x)
  def move(direction: Vector): Vector = Vector(x + direction.x, y + direction.y)

  def getTop: Vector = Vector(x, y + 1)
  def getRight: Vector = Vector(x + 1, y)
  def getBottom: Vector = Vector(x, y -1)
  def getLeft: Vector = Vector(x -1, y)

  def getTopRight: Vector = Vector(x + 1, y + 1)
  def getBottomRight: Vector = Vector(x + 1, y - 1)
  def getTopLeft: Vector = Vector(x - 1, y + 1)
  def getBottomLeft: Vector = Vector(x - 1, y - 1)

  def neighbours = Seq(getTop, getTopRight, getRight, getBottomRight, getBottom, getBottomLeft, getLeft, getTopLeft)

}
