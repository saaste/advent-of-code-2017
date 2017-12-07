import helpers.{ResultPrinter, Timer}
import models.{DiskInfo, DiskNode}

import scala.io.Source

object Day_7 extends App {

  val input = Source.fromResource("day-7.txt").getLines().toSeq
  val disks = parseInput(input)

  val result1 = Timer.time(findRoot(disks))
  val result2 = Timer.time {
    val rootDisk = findRoot(disks)
    val rootNode = buildTree(rootDisk, disks)
    findInvalidNode(rootNode)
  }

  ResultPrinter.printResult(result1, result2)

  def findRoot(disks: Seq[DiskInfo]): DiskInfo = {
    val withChilds = disks.filter(_.childNames.nonEmpty)
    withChilds.filterNot(d => withChilds.exists(_.childNames.contains(d.name))).head
  }

  def findInvalidNode(root: DiskNode, weightDifference: Int = 0): (DiskNode, Int, Int) = {
    if (root.hasChilds) {
      val groupsByTotalWeight = root.childNodes.groupBy(_.totalWeight)
      if (groupsByTotalWeight.size > 1) {
        val invalidNode = groupsByTotalWeight.minBy(_._2.length)._2.head

        if (weightDifference == 0) {
          val correctWeight = groupsByTotalWeight.keys.find(_ != invalidNode.totalWeight).get
          val diff = correctWeight - invalidNode.totalWeight
          findInvalidNode(invalidNode, diff)
        } else findInvalidNode(invalidNode, weightDifference)


      } else (root, weightDifference, root.weight + weightDifference)
    } else (root, weightDifference, root.weight + weightDifference)
  }

  private def buildTree(root: DiskInfo, disks: Seq[DiskInfo]): DiskNode = {
    val childs = disks.filter(d => root.childNames.contains(d.name)).map(child => buildTree(child, disks)).sortBy(_.totalWeight)
    DiskNode(root.name, root.weight, childs)
  }

  private def parseInput(input: Seq[String]): Seq[DiskInfo] = {
    val cleanedInput = input.map(_.replace("(", "").replace(")", "").replace("-> ", "").replace(",", "").split(" ").toSeq).filter(_.length >= 2)
    cleanedInput.map(splits => DiskInfo(splits.head, splits(1).toInt, splits.splitAt(2)._2))
  }
}


