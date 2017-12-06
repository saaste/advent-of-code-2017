import helpers.{ResultPrinter, Timer}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day_6 extends App {

  val input = Source.fromResource("day-6.txt").getLines().toArray.head.split("\\t").map(_.toInt)

  val result1 = Timer.time(calculateRedistributionCycles(input))
  val result2 = Timer.time(calculateLoops(result1.result._2.toArray))

  ResultPrinter.printResult(result1, result2)

  def calculateRedistributionCycles(input: Array[Int],
                                    usedArrays: ArrayBuffer[String] = ArrayBuffer.empty,
                                    steps: Int = 0): (Int, Seq[Int]) = {

    val inputId = input.mkString(".")
    if (usedArrays.contains(inputId)) return (steps, input.toSeq)

    val (largestBlock, largestIndex) = input.zipWithIndex.filter(_._1 == input.max).head
    usedArrays += inputId
    input(largestIndex) = 0

    spreadBlock(input, largestBlock, largestIndex)

    calculateRedistributionCycles(input, usedArrays, steps + 1)
  }

  def calculateLoops(input: Array[Int]): Int = {

    val requiredId = input.mkString(".")
    var steps = 0
    var currentId = ""

    while (currentId != requiredId) {
      val (largestBlock, largestIndex) = input.zipWithIndex.filter(_._1 == input.max).head
      input(largestIndex) = 0
      spreadBlock(input, largestBlock, largestIndex)
      steps += 1
      currentId = input.mkString(".")
    }

    steps
  }

  private def spreadBlock(input: Array[Int], blocks: Int, sourceIndex: Int): Unit = {
    var currentIndex = sourceIndex
    var blocksLeft = blocks

    while (blocksLeft > 0) {
      if (currentIndex + 1 > input.length -1) currentIndex = 0 else currentIndex += 1
      input(currentIndex) += 1
      blocksLeft -= 1
    }
  }

}
