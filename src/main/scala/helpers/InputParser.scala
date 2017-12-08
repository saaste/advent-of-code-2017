package helpers

import models.Instruction

object InputParser {

  def parseInstructions(input: Array[String]): Seq[Instruction] = {
    input.map(parseInstructionLine)
  }

  private def parseInstructionLine(line: String): Instruction = {
    val parts = line.split(" ")
    if (parts.length == 7) {
      val target = parts.head
      val op = parts(1)
      val amount = parts(2).toInt
      val ruleTarget = parts(4)
      val check = parts(5)
      val checkValue = parts(6).toInt
      Instruction(target, op, amount, ruleTarget, check, checkValue)
    } else throw new IllegalArgumentException("Parsing failed")
  }

}
