import helpers.InputParser
import models.Vector

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs

object Day_11 extends App {

  private val input = Source.fromResource("day-11.txt").getLines().mkString
  private val result1 = buildGrid(input)
  println(result1._2)
  println(result1._2.distanceFromZero) // 827 is too high

  val v1 = Vector(1, 1)
  val v2 = Vector(1, 1)
  println(v1 == v2)

  def buildGrid(input: String): (Seq[Hex], Hex) = {
    val hexes: ArrayBuffer[Hex] = ArrayBuffer(Hex(Vector(0, 0)))
    val moves: Seq[String] = input.split(',').map(_.trim)
    var currentHex = hexes.head

    moves.foreach { direction => // TODO: Slow. Filtering on every iter is not a good idea
      val nextHex = currentHex.getNeighbour(direction)
      if (!hexes.exists(_.location == nextHex.location)) hexes += nextHex
      currentHex = nextHex
    }

    (hexes, currentHex)
  }

}

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
    abs(location.x) + abs(location.y)
  }
}
