import helpers.{ResultPrinter, Timer}
import models.Node

import scala.io.Source

object Day_12 extends App {

  private val input = Source.fromResource("day-12.txt").getLines().toSeq
  private val links = parseConnections(input)

  val result1 = Timer.time(calculateConnections(links))
  val result2 = Timer.time(calculateNumberOfGroups(links))

  ResultPrinter.printResult(result1, result2)

  private def calculateConnections(nodes: Array[Node]): Int = {
    val root = nodes(0)
    root.markConnected()
    root.connectedCount()
  }

  private def calculateNumberOfGroups(nodes: Array[Node]): Int = {
    var groupId = 0
    while(nodes.count(_.groupId == -1) > 0) {
      val nonGroupNode = nodes.filter(_.groupId == -1).head
      nonGroupNode.setGroupId(groupId)
      groupId += 1
    }

    nodes.groupBy(_.groupId).size
  }

  private def parseConnections(input: Seq[String]): Array[Node] = {
    val connections = input.map { value =>
      val parts = value.trim.split("<->")
      if (parts.size != 2) throw new IllegalArgumentException
      else (parts.head.trim.toInt, parts(1).split(',').map(_.trim.toInt).toSeq)
    }.toMap

    val nodes: Array[Node] = (0 until connections.size).map(i => Node(i)).toArray
    connections.foreach { case (index, childIndexes) =>
      childIndexes.foreach(i => nodes(index).connections += nodes(i))
    }

    nodes
  }

}


