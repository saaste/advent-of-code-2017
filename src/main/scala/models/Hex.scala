package models

import scala.math.abs

case class Hex(location: Vector) {

  def getNeighbour(direction: String): Hex = direction.toLowerCase match {
    case "n" => Hex(Vector(location.x, location.y + 1))
    case "ne" => Hex(Vector(location.x + 1, location.y + 1))
    case "se" => Hex(Vector(location.x + 1, location.y - 1))
    case "s" => Hex(Vector(location.x, location.y - 1))
    case "sw" => Hex(Vector(location.x - 1, location.y -1))
    case "nw" => Hex(Vector(location.x - 1 , location.y + 1))
  }

  def distanceFromZero: Int = {
    if (location.x == location.y) abs(location.x) + abs(location.y)
    else Seq(abs(location.x), abs(location.y)).max
  }
}
