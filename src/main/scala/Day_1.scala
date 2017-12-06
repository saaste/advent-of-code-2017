import helpers.{ResultPrinter, Timer}

import scala.io.Source

object Day_1 extends App {

  val input = Source.fromResource("day-1.txt").getLines().mkString

  val result1 = Timer.time(calculateCaptchaNext(input))
  val result2 = Timer.time(calculateCaptchaHalf(input))

  ResultPrinter.printResult(result1, result2)

  def calculateCaptchaNext(input: String): Int = {
    val digits = input.map(_.asDigit)
    val validDigits = digits.zipWithIndex.flatMap { case (digit, index) =>
      val nextIndex = if (index < digits.length - 1) index + 1 else 0
      if (digit == digits(nextIndex)) Some(digit) else None
    }
    validDigits.sum
  }

  def calculateCaptchaHalf(input: String): Int = {
    val digits = input.map(_.asDigit)
    val half = digits.length / 2

    val validDigits = digits.zipWithIndex.flatMap { case (digit, index) =>
      val compareIndex = if (index + half <= digits.length - 1) index + half else index + half - digits.length
      if (digit == digits(compareIndex)) Some(digit) else None
    }
    validDigits.sum
  }


}
