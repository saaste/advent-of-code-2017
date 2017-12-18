
import helpers.{ResultPrinter, Timer}
import models._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day_18 extends App {

  private val input = Source.fromResource("day-18.txt").getLines().toArray

  val result1 = Timer.time {
    val instructions = DuetInstruction.parse(input)
    test(instructions)
  }

  val result2 = Timer.time {
    val instructions = DuetInstruction.parse(input)
    test2(instructions)
  }

  ResultPrinter.printResult(result1, result2)

  def test(instructions: Array[DuetInstruction]): Long = {
    val registers: Array[Long] = Array.fill(DuetInstruction.charToIndex.size)(0)

    var currentIndex: Int = 0
    var lastPlayedValue: Long = Long.MinValue
    var received: Boolean = false

    while (!received) {
      instructions(currentIndex) match {
        case o: Snd =>
          lastPlayedValue = o.run(registers)
          currentIndex += 1
        case o: Rcv =>
          received = o.isActivated(registers)
          currentIndex += 1
        case o: Jgz =>
          currentIndex += o.jump(registers)
        case o: EmptyRun =>
          o.run(registers)
          currentIndex += 1
      }

    }

    lastPlayedValue
  }

  def test2(instructions: Array[DuetInstruction]): Long = {
    val charToIndex: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap

    val registersA: Array[Long] = Array.fill(charToIndex.size)(0)
    registersA(charToIndex('p')) = 0

    val registersB: Array[Long] = Array.fill(charToIndex.size)(0)
    registersB(charToIndex('p')) = 1

    val registers: Array[Array[Long]] = Array(registersA, registersB)
    val queues: Array[ArrayBuffer[Long]] = Array(ArrayBuffer.empty, ArrayBuffer.empty)
    val currentIndices: Array[Int] = Array(0, 0)
    val sends: Array[Int] = Array(0, 0)

    var proc = 0
    var switchCount = 0

    def canContinue = switchCount < 10

    def target = if (proc == 0) 1 else 0

    def nextAndReset(): Unit = {
      currentIndices(proc) += 1
      switchCount = 0
    }

    while (canContinue) {

      instructions(currentIndices(proc)) match {
        case o: Snd =>
          queues(target) += o.run(registers(proc))
          sends(proc) += 1
          nextAndReset()
        case o: Rcv =>
          queues(proc).headOption match {
            case Some(i) =>
              registers(proc)(charToIndex(o.register)) = i
              queues(proc).remove(0)
              currentIndices(proc) += 1
            case None =>
              proc = target
              switchCount += 1
          }
        case o: Jgz =>
          currentIndices(proc) += o.jump(registers(proc))
        case o: EmptyRun =>
          o.run(registers(proc))
          nextAndReset()
      }

    }

    sends(1)
  }

}



