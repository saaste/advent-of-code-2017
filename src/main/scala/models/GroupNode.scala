package models

case class GroupNode() {
  var groupNumber: Int = 0
  var neighbours: Array[GroupNode] = _

  def setGroupNumber(groupNumber: Int): Unit = {
    this.groupNumber = groupNumber
    this.neighbours.filter(_.groupNumber != groupNumber).foreach(_.setGroupNumber(groupNumber))
  }
}
