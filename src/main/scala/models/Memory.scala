package models

case class Memory(items: Array[(Int, Vector)]) {
  def findDistance(value: Int): Int = items(value - 1)._2.distance
  def findFirstGreaterThan(value: Int): Int = items.filter(_._1 > value).map(_._1).min
}
