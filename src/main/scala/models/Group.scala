package models

case class Group(size: Int, child: Seq[Group]) {
  def sum: Int = {
    size + child.map(_.sum).sum
  }
}