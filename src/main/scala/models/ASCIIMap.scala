package models

import scala.util.Try

// TODO: Yeaaah... could do some cleanup but it is way too late and I am lazy AF
case class ASCIIMap(grid: Array[Array[Int]]) {
  private var location: Vector = Vector(grid(0).zipWithIndex.find(_._1 == ASCIIMap.VERTICAL).map(_._2).get, 0)
  private var direction: Vector = ASCIIMap.DOWN
  private var stepCount: Int = 0

  def current: Int = getCell(location).get

  def steps: Int = stepCount

  def move: Int = {
    if (current == ASCIIMap.EMPTY) current
    else if (current != ASCIIMap.CORNER) {
      location = location.move(direction)
      stepCount += 1
      current
    } else {
      if (direction == ASCIIMap.UP || direction == ASCIIMap.DOWN) {
        val left = location.move(ASCIIMap.LEFT)
        val right = location.move(ASCIIMap.RIGHT)
        val next =
          if (isRoute(left)) ASCIIMap.LEFT
          else if (isRoute(right)) ASCIIMap.RIGHT
          else throw new IllegalArgumentException()

        location = location.move(next)
        direction = next
        stepCount += 1
      } else {
        val up = location.move(ASCIIMap.UP)
        val down = location.move(ASCIIMap.DOWN)
        val next =
          if (isRoute(up)) ASCIIMap.UP
          else if (isRoute(down)) ASCIIMap.DOWN
          else throw new IllegalArgumentException()

        location = location.move(next)
        direction = next
        stepCount += 1
      }
      current
    }
  }

  private def isRoute(location: Vector): Boolean = {
    getCell(location).exists(i => !Array(ASCIIMap.CORNER, ASCIIMap.EMPTY).contains(i))
  }

  private def getCell(location: Vector): Option[Int] = {
    Try(grid(location.y)(location.x)).toOption
  }

}

object ASCIIMap {

  val DOWN: Vector = Vector(0, 1)
  val UP: Vector = Vector(0, -1)
  val RIGHT: Vector = Vector(1, 0)
  val LEFT: Vector = Vector(-1, 0)

  val VERTICAL: Int = '|'.toByte.toInt
  val HORIZONTAL: Int = '-'.toByte.toInt
  val CORNER: Int = '+'.toByte.toInt
  val EMPTY: Int = ' '.toByte.toInt

  val MARKERS: Array[Int] = Array(VERTICAL, HORIZONTAL, CORNER, EMPTY)
}
