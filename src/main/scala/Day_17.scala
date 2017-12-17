import helpers.{ResultPrinter, Timer}

import scala.collection.mutable.ArrayBuffer

object Day_17 extends App {

  val input = 386

  val result1 = Timer.time(test1(input))
  val result2 = Timer.time(test2(input))

  ResultPrinter.printResult(result1, result2)

  def test1(steps: Int): Int = {
    val array: ArrayBuffer[Int] = ArrayBuffer[Int](0)
    var currentIndex: Int = 0

    for (step <- 1 to 2017) {
      currentIndex = (currentIndex + steps) % array.size
      if (currentIndex == array.size -1) array += step else array.insert(currentIndex + 1, step)
      currentIndex += 1
    }

    array(currentIndex + 1)
  }

  def test2(steps: Int): Int = {
    var currentIndex: Int = 0
    var arrayLength: Int = 1
    var currentOne: Int = 0

    for (step <- 1 to 50000000) {
      currentIndex = (currentIndex + steps) % arrayLength
      if (currentIndex == 0) currentOne = step
      arrayLength += 1
      currentIndex += 1
    }

    currentOne
  }


}
