package models

import models.Directions.{DOWN, LEFT, RIGHT, UP}

import scala.collection.mutable

trait IGrid {

  protected var current: Vector = Vector(0, 0)
  protected var direction: Vector = UP
  protected var infections: Int = 0

  def move(): Unit = {
    turn()
    setState()
    current = current.move(direction)
  }

  def infectedBurst: Int = infections

  protected def turn(): Unit

  protected def setState(): Unit
}

case class InfiniteGrid(input: Map[Vector, Char]) extends IGrid {

  private val grid: mutable.Map[Vector, Char] = mutable.Map(input.toSeq: _*)

  private def isInfected(vector: Vector): Boolean = grid.get(vector).contains('#')

  protected def turn(): Unit = {
    direction = direction match {
      case UP => if (isInfected(current)) RIGHT else LEFT
      case RIGHT => if (isInfected(current)) DOWN else UP
      case DOWN => if (isInfected(current)) LEFT else RIGHT
      case LEFT => if (isInfected(current)) UP else DOWN
      case _ => throw new IllegalArgumentException
    }
  }

  protected def setState(): Unit = {
    grid(current) = if (grid.getOrElse(current, '.') == '.') {
      infections += 1
      '#'
    } else '.'
  }

}

case class InfiniteGrid2(input: Map[Vector, Char]) extends IGrid {

  private val grid: mutable.Map[Vector, Char] = mutable.Map(input.toSeq: _*)

  protected def turn(): Unit = {
    val state = grid.getOrElse(current, '.')
    direction = direction match {
      case UP => if (state == '#') RIGHT else if (state == 'F') DOWN else if (state == '.') LEFT else direction
      case RIGHT => if (state == '#') DOWN else if (state == 'F') LEFT else if (state == '.') UP else direction
      case DOWN => if (state == '#') LEFT else if (state == 'F') UP else if (state == '.') RIGHT else direction
      case LEFT => if (state == '#') UP else if (state == 'F') RIGHT else if (state == '.') DOWN else direction
      case _ => throw new IllegalArgumentException
    }
  }

  protected def setState(): Unit = {
    grid(current) = grid.getOrElse(current, '.') match {
      case '.' => 'W'
      case 'W' =>
        infections += 1
        '#'
      case '#' => 'F'
      case 'F' => '.'
      case _ => throw new IllegalArgumentException
    }
  }
}
