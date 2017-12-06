import helpers.Timer

import scala.io.Source

object Day_5 extends App {

  val input = Source.fromResource("day-5.txt").getLines().map(_.toInt).toSeq

  val t1 = Timer.start()
  val result1 = countStepsToExitFast(input)
  val ms1 = t1.stop()

  val t2 = Timer.start()
  val result2 = countStepsToExitFastAlternative(input)
  val ms2 = t2.stop()

  println(result1)
  println(s"In $ms1 ms")

  println("---------")

  println(result2)
  println(s"In $ms2 ms")

  def countStepsToExitFast(instructions: Seq[Int]): Int = {
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

  def countStepsToExitFastAlternative(instructions: Seq[Int]): Int = {
    val arr = instructions.toArray
    val lastIndex = arr.length - 1

    var currentIndex = 0
    var stepsTaken = 0

    while(currentIndex <= lastIndex) {
      val previousIndex = currentIndex
      currentIndex = currentIndex + arr(currentIndex)
      stepsTaken += 1
      val change = if (arr(previousIndex) >= 3) -1 else 1
      arr(previousIndex) += change
    }

    stepsTaken
  }

}
