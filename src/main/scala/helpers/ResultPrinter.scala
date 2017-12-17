package helpers

object ResultPrinter {

  def printResult[A, B](first: TimerResult[A], second: TimerResult[B]): Unit = {
    println("1st result: " + first.result)
    println(s"Solved in ${first.elapsed} ms")

    println("---------")

    println("2nd result: " + second.result)
    println(s"Solved in ${second.elapsed} ms")
  }

}
