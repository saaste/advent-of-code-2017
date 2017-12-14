import helpers.{ResultPrinter, Timer}
import models.GroupNode

object Day_14 extends App {

  val input = "xlqgujun"

  val result1 = Timer.time(test1(input))
  val result2 = Timer.time(test2(input))

  ResultPrinter.printResult(result1, result2)


  def test1(input: String): Int = {
    val rows = (0 to 127).map(i => hashToBinaryString(Day_10.calculateHash(Day_10.twistSecond(s"$input-$i")))).mkString
    rows.count(_ == '1')
  }

  def test2(input: String) = {
    val rows: Array[Array[Int]] = (0 to 127).map(i => hashToBinaryString(Day_10.calculateHash(Day_10.twistSecond(s"$input-$i"))).map(_.toString.toInt).toArray).toArray
    val nodes: Array[Array[GroupNode]] = rows.map(_.map(i => if (i == 1) GroupNode() else null))

    connectNodes(nodes)
    groupNodes(nodes)

    nodes.flatMap(_.filter(_ != null).map(_.groupNumber)).max
  }

  private def groupNodes(nodes: Array[Array[GroupNode]]): Unit = {
    var groupNumber = 1

    nodes.foreach { row =>
      row.foreach { node =>
        if (node != null && node.groupNumber == 0) {
          node.setGroupNumber(groupNumber)
          groupNumber += 1
        }
      }
    }

  }

  private def connectNodes(nodes: Array[Array[GroupNode]]): Unit = {
    for (row <- nodes.indices) {
      for (col <- nodes(row).indices) {
        if (nodes(row)(col) != null) nodes(row)(col).neighbours = findNeighbors(nodes, row, col)
      }
    }
  }

  private def findNeighbors(nodes: Array[Array[GroupNode]], row: Int, col: Int): Array[GroupNode] = {
    val top = if (row > 0 && nodes(row - 1)(col) != null) Some(nodes(row - 1)(col)) else None
    val right = if (col < nodes(row).length - 1 && nodes(row)(col + 1) != null) Some(nodes(row)(col + 1)) else None
    val bottom = if (row < nodes.length - 1 && nodes(row + 1)(col) != null) Some(nodes(row + 1)(col)) else None
    val left = if (col > 0 && nodes(row)(col - 1) != null) Some(nodes(row)(col - 1)) else None
    Array(top, right, bottom, left).flatten
  }

  private def hashToBinaryString(hash: String): String = {
    hash.map { char =>
      val paddedValue = toBinaryString(char)
      paddedValue
    }.mkString
  }


  private def toBinaryString(c: Char): String = {
    BigInt(c.toString, 16).toString(2).reverse.padTo(4, "0").reverse.mkString
  }

}
