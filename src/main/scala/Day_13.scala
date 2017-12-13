import helpers.{ResultPrinter, Timer}
import models.Scanner

import scala.io.Source

object Day_13 extends App {

  private val input = Source.fromResource("day-13.txt").getLines().toArray

  val result1 = Timer.time(findSeverity(input))
  val result2 = Timer.time(findWaitToPass(input))
  ResultPrinter.printResult(result1, result2)

  def findSeverity(input: Array[String], delay: Int = 0): Int = {

    val scannerInput = input.map { scannerInput =>
      val parts = scannerInput.split(':').map(_.trim.toInt)
      if (parts.length > 2) throw new IllegalArgumentException()
      (parts.head, parts(1))
    }.toMap
    val layers = scannerInput.keys.max


    val scanners = (0 to layers).map { i =>
      scannerInput.get(i) match {
        case Some(range) => Scanner(i, range)
        case None => Scanner.inactive
      }
    }

    var currentLocation = 0
    var severity = 0

    (0 until delay).foreach(_ => (0 to layers).foreach(scanners(_).move()))

    (0 to layers).foreach { i =>
      if (scanners(currentLocation).isWatching) {
        severity += i * scanners(currentLocation).getRange
      }
      (0 to layers).foreach(scanners(_).move())
      currentLocation += 1
    }

    severity
  }

  def findWaitToPass(input: Array[String]): Int = {
    import scala.util.control.Breaks._
    val scannerInput = input.map { scannerInput =>
      val parts = scannerInput.split(':').map(_.trim.toInt)
      if (parts.length > 2) throw new IllegalArgumentException()
      (parts.head, parts(1))
    }.toMap
    val layers = scannerInput.keys.max


    val scanners = (0 to layers).map { i =>
      scannerInput.get(i) match {
        case Some(range) => Scanner(i, range)
        case None => Scanner.inactive
      }
    }

    var isFailure = true
    var waitSteps = 0
    while (isFailure) {
      waitSteps += 1
      isFailure = false
      breakable { for (i <- scanners.indices) {
        if (scanners(i).isHitAfterNMoves(waitSteps)) {
          isFailure = true
          break
        }
      }
    }}

    waitSteps
  }

}
