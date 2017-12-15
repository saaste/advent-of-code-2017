import helpers.{ResultPrinter, Timer}

import scala.collection.mutable.ArrayBuffer

object Day_15 extends App {

  val result1 = Timer.time(test1())
  val result2 = Timer.time(test2())

  // Yeah, it is a brute force solution. Could be improved :(
  ResultPrinter.printResult(result1, result2)

  def test1(): Int = {
    var previousA: BigInt = 699
    var previousB: BigInt = 124
    var matches: Int = 0

    (1 to 40000000).foreach { _ =>
      previousA = (previousA * 16807) % 2147483647
      previousB = (previousB * 48271) % 2147483647

      if (getBits(previousA) == getBits(previousB)) matches += 1
    }

    matches
  }

  def test2(): Int = {
    var previousA: BigInt = 699
    var previousB: BigInt = 124

    var aNumbers = ArrayBuffer.empty[BigInt]
    var bNumbers = ArrayBuffer.empty[BigInt]


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
      if (getBits(aNumbers(i)) == getBits(bNumbers(i))) matches += 1
    }

    matches


  }

  private def getBits(i: BigInt, significantBits: Int = 16): BigInt = {
    i & ((1 << significantBits) - 1)
  }

}
