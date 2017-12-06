import helpers.{ResultPrinter, Timer}

import scala.io.Source

object Day_5 extends App {

  val input = Source.fromResource("day-5.txt").getLines().map(_.toInt).toSeq

  val result1 = Timer.time(countStepsToExit(input))
  val result2 = Timer.time(countStepsToExitAlternative(input))

  ResultPrinter.printResult(result1, result2)

  def countStepsToExit(instructions: Seq[Int]): Int = {
    val arr = instructions.toArray
    val lastIndex = arr.length - 1

    var currentIndex = 0
    var stepsTaken = 0

    while(currentIndex <= lastIndex) {
      val previousIndex = currentIndex
      currentIndex = currentIndex + arr(currentIndex)
      stepsTaken += 1
      arr(previousIndex) += 1
    }

    stepsTaken
  }

  def countStepsToExitAlternative(instructions: Seq[Int]): Int = {
    val arr = instructions.toArray
    val lastIndex = arr.length - 1

    var currentIndex = 0
    var stepsTaken = 0

    while(currentIndex <= lastIndex) {
      val previousIndex = currentIndex
      currentIndex = currentIndex + arr(currentIndex)
      stepsTaken += 1
      arr(previousIndex) += (if (arr(previousIndex) >= 3) -1 else 1)
    }

    stepsTaken
  }

}
