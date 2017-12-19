import helpers.{ResultPrinter, Timer}
import models.ASCIIMap

import scala.io.Source

object Day_19 extends App {

  private val input = Source.fromResource("day-19.txt").getLines().toArray

  val result1 = Timer.time {
    val map = parseInput(input)
    test1(map)
  }

  val result2 = Timer.time {
    val map = parseInput(input)
    test2(map)
  }

  ResultPrinter.printResult(result1, result2)


  def test1(map: ASCIIMap): String = {
    var current = map.current
    var letters = ""

    while (current != ASCIIMap.EMPTY) {
      map.move
      current = map.current
      if (!ASCIIMap.MARKERS.contains(current)) letters += current.toChar.toString
    }

    letters
  }

  def test2(map: ASCIIMap): Int = {
    var current = map.current

    while (current != ASCIIMap.EMPTY) {
      map.move
      current = map.current
    }

    map.steps
  }

  private def parseInput(input: Array[String]): ASCIIMap = {
    val grid = input.map { row => row.map(_.toByte.toInt).toArray }
    ASCIIMap(grid)
  }

}
