import helpers.{ResultPrinter, Timer}

import scala.io.Source

object Day_4 extends App {

  val input = Source.fromResource("day-4.txt").getLines().toSeq

  val result1 = Timer.time(input.count(isValid))
  val result2 = Timer.time(input.count(isSecureValid))

  ResultPrinter.printResult(result1, result2)

  def isValid(passPhrase: String): Boolean = {
    val words = passPhrase.split(" ")
    words.length == words.distinct.length
  }

  def isSecureValid(passPhrase: String): Boolean = {
    val words = passPhrase.split(" ").toSeq.map(_.toCharArray.sorted.mkString)
    words.length == words.distinct.length
  }


}
