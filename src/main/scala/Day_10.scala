import helpers.Converters._
import helpers.{ResultPrinter, Timer}
import models.CircularList

import scala.io.Source

object Day_10 extends App {

  private val input = Source.fromResource("day-10.txt").getLines().mkString

  val result1 = Timer.time(multiply(twistFirst(input)))
  val result2 = Timer.time(calculateHash(twistSecond(input)))

  ResultPrinter.printResult(result1, result2)

  def twistFirst(inputString: String): Seq[Int] = {
    val input = inputString.trim.split(',').map(_.toInt)
    val sequence = CircularList(0 to 255)
    var skipSize = 0

    input.foreach { length =>
      sequence.replaceWithValues(sequence.selectItems(length).reverse)
      sequence.move(length + skipSize)
      skipSize += 1
    }

    sequence.values
  }

  def twistSecond(inputString: String): Seq[Int] = {
    val input = inputString.trim.map(_.toByte.toInt) ++ Array(17, 31, 73, 47, 23)
    val sequence = CircularList(0 to 255)
    var skipSize = 0

    for (_ <- 1 to 64) {
      input.foreach { length =>
        sequence.replaceWithValues(sequence.selectItems(length).reverse)
        sequence.move(length + skipSize)
        skipSize += 1
      }
    }
    sequence.values
  }

  def multiply(input: Seq[Int]): Int = input.head * input(1)

  def calculateHash(input: Seq[Int]): String = {
    val chunks = input.grouped(16).toSeq
    val denseHash = chunks.map(_.foldLeft(0)((b, a) => b ^ a)).toArray
    denseHash.map(_.toHex).mkString
  }

}