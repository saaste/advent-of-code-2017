import helpers.{ResultPrinter, Timer}

import scala.collection.mutable.ArrayBuffer

object Day_15 extends App {

  val result1 = Timer.time(test1())
  val result2 = Timer.time(test2())

  // Yeah, it is a brute force solution. Could be improved :(
  ResultPrinter.printResult(result1, result2)

  def test1(): Int = {
    var previousA: Long = 699
    var previousB: Long = 124
    var matches: Int = 0

    (1 to 40000000).foreach { _ =>
      previousA = (previousA * 16807L) % 2147483647L
      previousB = (previousB * 48271L) % 2147483647L

      if (previousA.toShort == previousB.toShort) matches += 1
    }

    matches
  }

  def test2(): Int = {
    var previousA: Long = 699
    var previousB: Long = 124

    var aNumbers = ArrayBuffer.empty[Long]
    var bNumbers = ArrayBuffer.empty[Long]


    while (aNumbers.length < 5000000) {
      previousA = (previousA * 16807) % 2147483647
      if (previousA % 4 == 0) aNumbers += previousA
    }

    while (bNumbers.length < 5000000) {
      previousB = (previousB * 48271) % 2147483647
      if (previousB % 8 == 0) bNumbers += previousB
    }

    var matches: Int = 0

    aNumbers.indices.foreach { i =>
      if (aNumbers(i).toShort == bNumbers(i).toShort) matches += 1
    }

    matches


  }

}
