import helpers.{ResultPrinter, Timer}
import models.PatternCanvas

import scala.io.Source

// TODO: Oh man, this is a mess. Result is correct but implementation a slow clusterfuck
object Day_21 extends App {

  private val input = Source.fromResource("day-21.txt").getLines().toArray
  val initialInput = ".#./..#/###"

  val result1 = Timer.time {
    val rules = parseRules(input)
    val canvas = PatternCanvas(initialInput, rules)
    for (_ <- 1 to 5) canvas.split()
    canvas.openPixels
  }

  val result2 = Timer.time {
    val rules = parseRules(input)
    val canvas = PatternCanvas(initialInput, rules)
    for (_ <- 1 to 18) canvas.split()
    canvas.openPixels
  }

  ResultPrinter.printResult(result1, result2)


  private def parseRules(input: Array[String]): Map[String, String] = {
    input.map { line =>
      val parts = line.split(" => ")
      (parts.head, parts(1))
    }.toMap
  }

}
