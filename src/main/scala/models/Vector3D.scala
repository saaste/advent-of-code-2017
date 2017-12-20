package models

import scala.math.abs

case class Vector3D(var x: Int, var y: Int, var z: Int) {

  def add(o: Vector3D): Unit = {
    x += o.x
    y += o.y
    z += o.z
  }

  def distance: Int = abs(x) + abs(y) + abs(z)

  def isSameDirection(that: Vector3D): Boolean = {
    if (x == 0 || y == 0 || z == 0) {
      x == that.x && y == that.y && z == that.z
    } else {
      val xProportion = that.x / x
      val yProportion = that.y / y
      val zProportion = that.z / z
      xProportion == yProportion && yProportion == zProportion
    }

  }

  override def equals(o: scala.Any): Boolean = o match {
    case t: Vector3D => t.x == x && t.y == y && t.z == z
    case _ => false
  }

  def >(t: Vector3D): Boolean = distance > t.distance

  def <(t: Vector3D): Boolean = distance < t.distance
}