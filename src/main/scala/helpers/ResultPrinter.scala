package helpers

object ResultPrinter {

  def printResult[A, B](first: TimerResult[A], second: TimerResult[B]): Unit = {
    println(first.result)
    println(s"In ${first.elapsed} ms")

    println("---------")

    println(second.result)
    println(s"In ${second.elapsed} ms")
  }

}
