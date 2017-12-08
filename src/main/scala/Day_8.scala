import helpers.{InputParser, ResultPrinter, Timer, TimerResult}
import models.Instruction

import scala.collection.mutable
import scala.io.Source

object Day_8 extends App {

  private val input = Source.fromResource("day-8.txt").getLines().toArray
  private val instructions = InputParser.parseInstructions(input)

  val result = Timer.time(findLargestValue(instructions))
  ResultPrinter.printResult(result, TimerResult(None, 0))

  def findLargestValue(instructions: Seq[Instruction]): (Int, Int) = {
    val values: mutable.Map[String, Int] = mutable.Map(instructions.map(_.target).distinct.map(name => (name, 0)):_*)
    var highestValue = Int.MinValue

    instructions.foreach { instruction =>
      if (isValid(instruction, values(instruction.ruleTarget), instruction.checkValue)) {
        if (instruction.op == "inc") {
          values(instruction.target) += instruction.amount
        } else {
          values(instruction.target) -= instruction.amount
        }
        if (values(instruction.target) > highestValue) highestValue = values(instruction.target)
      }
    }
    (values.values.max, highestValue)
  }

  private def isValid(instruction: Instruction, value: Int, checkValue: Int): Boolean = {
    instruction.check match {
      case "<" => value < checkValue
      case "<=" => value <= checkValue
      case "==" => value == checkValue
      case ">=" => value >= checkValue
      case ">" => value > checkValue
      case "!=" => value != checkValue
      case _ => throw new IllegalArgumentException(s"Invalid instruction ${instruction.check}")
    }
  }

}
