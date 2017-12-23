import helpers.{ResultPrinter, Timer}
import models.{IGrid, InfiniteGrid, InfiniteGrid2, Vector}

import scala.io.Source
import scala.math.floor
object Day_22 extends App {

  private val input = Source.fromResource("day-22.txt").getLines().toArray

  val result1 = Timer.time {
    val grid = parseInfiniteGrid(input, 1)
    (1 to 10000).foreach(_ => grid.move())
    grid.infectedBurst
  }

  val result2 = Timer.time {
    val grid = parseInfiniteGrid(input, 2)
    (1 to 10000000).foreach(_ => grid.move())
    grid.infectedBurst
  }

  ResultPrinter.printResult(result1, result2)

  def parseInfiniteGrid(input: Array[String], version: Int): IGrid = {
    val rows = input.length
    val cols = input.head.length

    val rowIndexFix = floor(rows / 2.0).toInt
    val colIndexFix = floor(cols / 2.0).toInt

    val map = input.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.map { case (char, colIndex) =>
        val newRow = rowIndexFix - rowIndex
        val newCol = colIndex - colIndexFix
        (Vector(newCol, newRow), char)
      }
    }.toMap

    if (version == 1) InfiniteGrid(map) else InfiniteGrid2(map)

  }

}

