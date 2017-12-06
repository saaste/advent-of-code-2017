import helpers.Timer

import scala.io.Source

object Day_4 extends App {

  val input = Source.fromResource("day-4.txt").getLines().toSeq

  val t1 = Timer.start()
  val result1 = input.count(isValid)
  val ms1 = t1.stop()

  val t2 = Timer.start()
  val result2 = input.count(isSecureValid)
  val ms2 = t2.stop()

  println(result1)
  println(s"In $ms1 ms")

  println("---------")

  println(result2)
  println(s"In $ms2 ms")

  def isValid(passPhrase: String): Boolean = {
    val words = passPhrase.split(" ")
    words.length == words.distinct.length
  }

  def isSecureValid(passPhrase: String): Boolean = {
    val words = passPhrase.split(" ").toSeq.map(_.toCharArray.sorted.mkString)
    words.length == words.distinct.length
  }


}
