package models

import models.DuetInstruction.charToIndex

import scala.util.{Success, Try}

trait EmptyRun {
  def run(registers: Array[Long]): Unit
}

trait DuetInstruction {
  protected def getValue(source: Either[Char, Long], registers: Array[Long]): Long = source match {
    case Left(c) => registers(charToIndex(c))
    case Right(i) => i
  }
}

object DuetInstruction {

  val charToIndex: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap

  def parse(input: Array[String]): Array[DuetInstruction] = {
    input.map(DuetInstruction.parse)
  }

  private def parse(s: String): DuetInstruction = {
    val parts = s.split(' ').map(_.trim)

    val cmd = parts.head
    val register = parseRegisterValue(parts)
    val value = parseValue(parts)

    cmd match {
      case "snd" => Snd(register.left.get)
      case "set" => Set(register.left.get, value.get)
      case "add" => Add(register.left.get, value.get)
      case "mul" => Mul(register.left.get, value.get)
      case "mod" => Mod(register.left.get, value.get)
      case "rcv" => Rcv(register.left.get)
      case "jgz" => Jgz(register, value.get)
      case e => throw new IllegalArgumentException(s"Invalid instruction $e")
    }
  }

  private def parseRegisterValue(parts: Array[String]): Either[Char, Int] = {
    Try(parts(1)(0).toString.toInt) match {
      case Success(i) => Right(i)
      case _ => Left(parts(1)(0))
    }
  }

  private def parseValue(parts: Array[String]): Option[Either[Char, Long]] = {
    if (parts.length == 3) Some {
      Try(parts(2).toLong) match {
        case Success(i) => Right(i)
        case _ => Left(parts(2)(0))
      }
    } else None
  }

}

case class Snd(register: Char) extends DuetInstruction {
  def run(registers: Array[Long]): Long = registers(charToIndex(register))
}

case class Set(register: Char, value: Either[Char, Long]) extends DuetInstruction with EmptyRun {
  def run(registers: Array[Long]): Unit = registers(charToIndex(register)) = getValue(value, registers)
}

case class Add(register: Char, increase: Either[Char, Long]) extends DuetInstruction with EmptyRun {
  def run(registers: Array[Long]): Unit = registers(charToIndex(register)) += getValue(increase, registers)
}

case class Mul(register: Char, multiplier: Either[Char, Long]) extends DuetInstruction with EmptyRun {
  def run(registers: Array[Long]): Unit = registers(charToIndex(register)) *= getValue(multiplier, registers)
}

case class Mod(register: Char, divider: Either[Char, Long]) extends DuetInstruction with EmptyRun {
  def run(registers: Array[Long]): Unit = registers(charToIndex(register)) %= getValue(divider, registers)
}

case class Rcv(register: Char) extends DuetInstruction {
  def isActivated(registers: Array[Long]): Boolean = {
    registers(charToIndex(register)) != 0
  }
}

case class Jgz(register: Either[Char, Int], offset: Either[Char, Long]) extends DuetInstruction {
  def jump(registers: Array[Long]): Int = {

    val offsetValue = offset match {
      case Left(c) => registers(charToIndex(c))
      case Right(i) => i
    }

    val compareValue = register match {
      case Left(c) => registers(charToIndex(c))
      case Right(i) => i.toLong
    }

    if (compareValue > 0) offsetValue.toInt else 1
  }
}
