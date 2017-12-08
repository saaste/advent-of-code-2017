package models

case class Instruction(target: String, op: String, amount: Int, ruleTarget: String, check: String, checkValue: Int)
