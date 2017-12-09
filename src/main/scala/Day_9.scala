import helpers.{ResultPrinter, Timer, TimerResult}
import models.Group

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day_9 extends App {

  private val input = Source.fromResource("day-9.txt").getLines().mkString

  val result = Timer.time {
    val cleanedInput = cleanFromGarbage(input)
    val result = parseGroupInput(cleanedInput._1)
    (result.sum, cleanedInput._2)
  }

  ResultPrinter.printResult(result, TimerResult(None, 0))

  def cleanFromGarbage(input: String): (String, Int) = {
    var cleanedInput: String = ""
    var removedChars: Int = 0
    var ignoreNext: Boolean = false
    var garbageActive = false

    input.foreach { c =>
      if (!ignoreNext) {
        c match {
          case '<' => if (garbageActive) removedChars += 1 else garbageActive = true
          case '!' => ignoreNext = true
          case '>' => garbageActive = false
          case _ if !garbageActive => cleanedInput += c
          case _ => removedChars += 1
        }
      } else ignoreNext = false
    }

    (cleanedInput, removedChars)
  }

  def parseGroupInput(cleanedString: String, groupId: Int = 1): Group = {

    val groupInputs = ListBuffer[String]()
    var openBrackets = 0
    var currentGroupInput = ""

    if (cleanedString.length > 0) {
      cleanedString.substring(1, cleanedString.length() - 1).foreach { c =>
        c match {
          case '{' =>
            openBrackets += 1
            currentGroupInput += c
          case '}' =>
            openBrackets -= 1
            currentGroupInput += c
          case _ if openBrackets > 0 => currentGroupInput += c
          case _ => // ignore
        }

        if (openBrackets == 0) {
          if (currentGroupInput.nonEmpty) {
            groupInputs += currentGroupInput
            currentGroupInput = ""
          }
        }
      }
    }

    Group(groupId, groupInputs.map(parseGroupInput(_, groupId + 1)))
  }

}
