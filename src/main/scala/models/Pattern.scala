package models

trait Pattern {
  def rotateCW(times: Int): Array[Array[Char]]
  def flipVertical(): Array[Array[Char]]
  def flipHorizontal(): Array[Array[Char]]
  def isMatch(input: String): Boolean

  def cellToString(cells: Array[Array[Char]]): String = cells.map(_.mkString).mkString("/")
}

object Pattern {
  def apply(s: String): Pattern = s.length match {
    case 11 => Pattern3x3(s)
    case 5 => Pattern2x2(s)
    case _ => throw new IllegalArgumentException(s"Invalid input $s")
  }


}

case class Pattern2x2(input: String) extends Pattern {
  if (input.length != 5) throw new IllegalArgumentException(s"$input is not divisible by 2")

  private val cells: Array[Array[Char]] = input.split('/').map(_.toCharArray)

  private val stringPattern = Array(
    cellToString(cells),
    cellToString(flipVertical()),
    cellToString(flipHorizontal()),
    cellToString(rotateCW(1)),
    cellToString(rotateCW(2)),
    cellToString(rotateCW(3)),
    cellToString(rotateCW(1).reverse),
    cellToString(rotateCW(3).reverse))

  override def flipVertical(): Array[Array[Char]] = cells.reverse
  override def flipHorizontal(): Array[Array[Char]] = cells.map(_.reverse)

  override def rotateCW(times: Int): Array[Array[Char]] = {
    var rotatedCells = cells.clone()

    for (_ <- 1 to times) {
      val row1 = Array(rotatedCells(1)(0), rotatedCells(0)(0))
      val row2 = Array(rotatedCells(1)(1), rotatedCells(0)(1))
      rotatedCells = Array(row1, row2)
    }

    rotatedCells
  }

  def convert(rules: Map[String, String]): Array[Array[Char]] = {
    rules.find { case (pattern, _) => isMatch(pattern) } match {
      case Some((_, target)) => target.split('/').map(_.toCharArray)
      case None => throw new IllegalArgumentException("No matching rule!")
    }
  }

  override def isMatch(input: String): Boolean = {
    if (input.length == 5) stringPattern.contains(input) else false
  }

  override def toString: String = cellToString(cells).split('/').mkString("\n")

}

case class Pattern3x3(input: String) extends Pattern {
  if (input.length != 11) throw new IllegalArgumentException(s"$input is not divisible by 3")

  private val cells: Array[Array[Char]] = input.split('/').map(_.toCharArray)

  private val stringPattern = Array(
    cellToString(cells),
    cellToString(flipVertical()),
    cellToString(flipHorizontal()),
    cellToString(rotateCW(1)),
    cellToString(rotateCW(2)),
    cellToString(rotateCW(3)),
    cellToString(rotateCW(1).reverse),
    cellToString(rotateCW(3).reverse)
  )

  override def flipVertical(): Array[Array[Char]] = cells.reverse
  override def flipHorizontal(): Array[Array[Char]] = cells.map(_.reverse)
  override def rotateCW(times: Int): Array[Array[Char]] = {
    var rotatedCells = cells.clone()

    for (_ <- 1 to times) {
      val row1 = Array(rotatedCells(2)(0), rotatedCells(1)(0), rotatedCells(0)(0))
      val row2 = Array(rotatedCells(2)(1), rotatedCells(1)(1), rotatedCells(0)(1))
      val row3 = Array(rotatedCells(2)(2), rotatedCells(1)(2), rotatedCells(0)(2))
      rotatedCells = Array(row1, row2, row3)
    }

    rotatedCells
  }

  def convert(rules: Map[String, String]): Array[Array[Char]] = {
    rules.find { case (pattern, _) => isMatch(pattern) } match {
      case Some((_, target)) => target.split('/').map(_.toCharArray)
      case None => throw new IllegalArgumentException("No matching rule!")
    }
  }

  override def toString: String = cellToString(cells).split('/').mkString("\n")

  override def isMatch(input: String): Boolean = {
    if (input.length == 11) stringPattern.contains(input) else false
  }

}