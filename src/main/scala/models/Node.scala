package models

import scala.collection.mutable.ArrayBuffer

case class Node(index: Int, var connections: ArrayBuffer[Node] = ArrayBuffer.empty) {

  private var _isConnected = false
  private var _groupId = -1

  def isConnected: Boolean = _isConnected
  def groupId: Int = _groupId

  def markConnected(): Unit = {
    _isConnected = true
    connections.filterNot(_.isConnected).foreach(_.markConnected())
  }

  def setGroupId(id: Int): Unit = {
    _groupId = id
    connections.filter(_.groupId != id).foreach(_.setGroupId(id))

  }


  def connectedCount(visited: ArrayBuffer[Node] = ArrayBuffer.empty): Int = {
    if (!visited.contains(this)) visited += this

    val nextChilds = connections.filterNot(visited.contains(_)).filter(_.isConnected)
    if (nextChilds.nonEmpty) {
      val res = nextChilds.map(_.connectedCount(visited)).max
      res
    }
    else visited.size
  }

}
