import helpers.{ResultPrinter, Timer}
import models.{DanceMove, Exchange, Partner, Spin}

import scala.io.Source

object Day_16 extends App {

  private val input = Source.fromResource("day-16.txt").getLines().toArray.head.mkString

  val result1 = Timer.time(test1(input))
  val result2 = Timer.time(test2(input))

  ResultPrinter.printResult(result1, result2)

  def test1(input: String): String = {
    val programs = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
    val moves = parseInput(input)
    dance(programs, moves)
    programs.mkString
  }

  def test2(input: String): String = {
    val programs = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')

    val moves = parseInput(input)
    val loops = 1000000000 % movesToFullRotation(programs, moves)

    (1 until loops).foreach(_ => dance(programs, moves))
    dance(programs, moves)

    programs.mkString
  }

  private def movesToFullRotation(programs: Array[Char], moves: Array[DanceMove]): Int = {
    val clone = programs.clone()

    var counter = 1
    dance(clone, moves)

    while (!clone.sameElements(programs)) {
      dance(clone, moves)
      counter += 1
    }

    counter
  }

  private def dance(programs: Array[Char], moves: Array[DanceMove]): Unit = {
    moves.foreach {
      case m: Spin => spin(programs, m)
      case m: Exchange => exchange(programs, m)
      case m: Partner => partner(programs, m)
    }
  }

  private def parseInput(input: String): Array[DanceMove] = {
    val parts = input.split(',')
    parts.map { danceMove =>
      danceMove.head match {
        case 's' =>
          Spin(danceMove.substring(1).trim.toInt)
        case 'x' =>
          val positions = danceMove.substring(1).split('/')
          Exchange(positions.head.trim.toInt, positions(1).trim.toInt)
        case 'p' =>
          val programs = danceMove.substring(1).split('/')
          Partner(programs.head.head, programs(1).head)
        case invalid => throw new IllegalArgumentException(s"Move: $danceMove")
      }
    }
  }

  private def spin(programs: Array[Char], danceMove: Spin): Unit = {
    val originalArray = programs.clone()
    programs.indices.foreach { originalIndex =>
      val newIndex = (originalIndex + danceMove.steps) % programs.length
      programs(newIndex) = originalArray(originalIndex)
    }
  }

  private def exchange(programs: Array[Char], danceMove: Exchange): Unit = {
    val originalA = programs(danceMove.a)
    val originalB = programs(danceMove.b)
    programs(danceMove.a) = originalB
    programs(danceMove.b) = originalA
  }

  private def partner(programs: Array[Char], danceMove: Partner): Unit = {
    val indexA = programs.indexOf(danceMove.a)
    val indexB = programs.indexOf(danceMove.b)
    val originalA = programs(indexA)
    val originalB = programs(indexB)
    programs(indexA) = originalB
    programs(indexB) = originalA
  }

}
