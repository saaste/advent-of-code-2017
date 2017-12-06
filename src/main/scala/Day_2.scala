import helpers.{ResultPrinter, Timer}

import scala.io.Source

object Day_2 extends App {

  val input = Source.fromResource("day-2.txt").getLines().mkString("\n")
  val spreadSheet = inputToSpreadSheet(input)

  val result1 = Timer.time(calculateChecksum(spreadSheet))
  val result2 = Timer.time(calculateDivideChecksum(spreadSheet))

  ResultPrinter.printResult(result1, result2)

  def calculateChecksum(spreadSheet: Seq[Seq[Int]]): Int = {
    spreadSheet.map { cells =>
      cells.max - cells.min
    }.sum
  }

  def calculateDivideChecksum(spreadSheet: Seq[Seq[Int]]): Int = {
    spreadSheet.map { cells =>
      findDivideResult(cells.sorted.reverse)
    }.sum
  }

  private def findDivideResult(values: Seq[Int]): Int = {
    if (values.length > 1) {
      val current = values.head
      val comparisons = values.tail

      for (i <- comparisons.indices) {
        if (current % comparisons(i) == 0) return current / comparisons(i)
      }

      findDivideResult(comparisons)
    } else 0
  }

  private def inputToSpreadSheet(input: String): Seq[Seq[Int]] = {
    input.trim.split("\\n").toSeq.map(_.split("\\t").toSeq.map(_.toInt))
  }

}
