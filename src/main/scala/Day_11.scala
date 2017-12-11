import helpers.{ResultPrinter, Timer}
import models.{Hex, Vector}

import scala.collection.mutable
import scala.io.Source


object Day_11 extends App {

  private val input = Source.fromResource("day-11.txt").getLines().mkString
  private val result1 = Timer.time(buildGrid(input)._2.distanceFromZero)
  private val result2 = Timer.time(buildGrid(input)._1.map(_.distanceFromZero).max)
  ResultPrinter.printResult(result1, result2)

  def buildGrid(input: String): (Seq[Hex], Hex) = {
    val hexes: mutable.Map[String, Hex] = mutable.Map("0,0" -> Hex(Vector(0, 0)))
    val moves: Seq[String] = input.split(',').map(_.trim)
    var currentHex = hexes.head._2

    moves.foreach { direction =>
      val nextHex = currentHex.getNeighbour(direction)
      hexes(nextHex.location.toString) = nextHex
      currentHex = nextHex
    }

    (hexes.values.toSeq, currentHex)
  }

}


