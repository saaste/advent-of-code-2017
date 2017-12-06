import helpers.Timer
import models._

object Day_3 extends App {

  val input = 312051

  val t1 = Timer.start()
  val memory1 = buildMemory(input)
  val result1 = memory1.findDistance(input)
  val ms1 = t1.stop()

  val t2 = Timer.start()
  val memory2 = buildIncreasingMemory(input)
  val result2 = memory2.findFirstGreaterThan(input)
  val ms2 = t2.stop()

  println(result1)
  println(s"In $ms1 ms")

  println("---------")

  println(result2)
  println(s"In $ms2 ms")


  // TODO: Runs in ~400 ms. There probably is better way to do this
  def buildMemory(size: Int, fillWithZeroes: Boolean = false): Memory = {

    val vectors = new Array[(Int, Vector)](size)
    vectors(0) = (1, Vector(0, 0))

    var currentPosition = Vector(0, 0)
    var direction = Vector(1, 0)

    var requiredMoves = 1
    var executedMoves = 0
    var executedTurns = 0

    (2 to size).foreach { value =>

      currentPosition = currentPosition.move(direction)
      executedMoves += 1

      val valueToStore = if (fillWithZeroes) 0 else value
      vectors(value - 1) = (valueToStore, currentPosition)

      if (executedMoves == requiredMoves) {
        direction = direction.rotateCCW
        executedTurns += 1
        executedMoves = 0
        if (executedTurns == 2) {
          executedTurns = 0
          requiredMoves += 1
        }
      }
    }

    Memory(vectors)
  }

  // TODO: Runs in ~2,7 s. There probably is better way to do this
  def buildIncreasingMemory(size: Int): Memory = {

    val vectors = buildMemory(size, fillWithZeroes = true).items
    val coordinates = vectors.zipWithIndex.map {
      case ((_, vector), index) => (vector, index)
    }.toMap

    for (i <- vectors.indices) {
      val newValue = getNeighboursSum(vectors(i)._2, vectors, coordinates)
      vectors(i) = (newValue, vectors(i)._2)
    }

    Memory(vectors)
  }

  private def getNeighboursSum(current: Vector, vectors: Array[(Int, Vector)], coordinates: Map[Vector, Int]): Int = {
    if (current != Vector(0, 0)) {
      current.neighbours.map { vector =>
        coordinates.get(vector).map(index => vectors(index)._1).getOrElse(0)
      }.sum
    } else 1
  }

}
