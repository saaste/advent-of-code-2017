package models

case class DiskInfo(name: String, weight: Int, childNames: Seq[String])

case class DiskNode(name: String, weight: Int, childNodes: Seq[DiskNode]) {
  val totalWeight: Int = weight + childNodes.map(_.totalWeight).sum
  val hasChilds: Boolean = childNodes.nonEmpty

  override def toString: String = name

  def printTree(level: Int = 0): Unit = {
    val spacing = " " * level
    println(s"$spacing$name ($totalWeight)")
    childNodes.foreach(_.printTree(level + 1))
  }
}
