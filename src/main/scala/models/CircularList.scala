package models

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case class CircularList[T](private val inputValues: Seq[T])(implicit tag: ClassTag[T]) {

  private var currentIndex = 0
  private val internalValues = inputValues.toArray.clone()

  def values: Seq[T] = internalValues

  def move(steps: Int): Unit = {
    for (_ <- 1 to steps) {
      currentIndex = nextIndex(1)
    }
  }

  def selectItems(n: Int): Seq[T] = {
    if (n > internalValues.length) throw new IndexOutOfBoundsException(n.toString)

    val selectedItems = ArrayBuffer[T]()
    var steps = 1
    var index = currentIndex

    for (_ <- 1 to n) {
      selectedItems += internalValues(index)
      index = nextIndex(steps)
      steps += 1
    }

    selectedItems
  }

  def replaceWithValues(newValues: Seq[T]): Unit = {
    if (newValues.length > internalValues.length) throw new IndexOutOfBoundsException(newValues.length.toString)

    var index = currentIndex
    var steps = 1

    newValues.foreach { newValue =>
      internalValues(index) = newValue
      index = nextIndex(steps)
      steps += 1
    }
  }

  private def nextIndex(steps: Int): Int = {
    if (currentIndex + steps > internalValues.length - 1) currentIndex + steps - internalValues.length else currentIndex + steps
  }

}