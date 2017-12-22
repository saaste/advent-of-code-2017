package models

case class PatternCanvas(initialInput: String, rules: Map[String, String]) {

  private var canvas: Array[Array[Char]] = initialInput.split('/').map(_.toCharArray)

  def openPixels: Int = canvas.map(_.count(_ == '#')).sum

  def split(): Unit = {
    if (canvas.length % 2 == 0) {
      val subGrids = splitSubGrid(canvas, 2).map(_.map(Pattern2x2))
      val convertedGrids = subGrids.map(_.map(_.convert(rules)))
      val result = convertedGrids.flatMap { gridRow =>
        (0 to 2).map { mainRow =>
          gridRow.flatMap(subRows => subRows(mainRow))
        }
      }
      canvas = result

    } else if (canvas.length % 3 == 0) {
      val subGrids = splitSubGrid(canvas, 3).map(_.map(Pattern3x3))
      val convertedGrids = subGrids.map(_.map(_.convert(rules)))
      val result = convertedGrids.flatMap { gridRow =>
        (0 to 3).map { mainRow =>
          gridRow.flatMap(subRows => subRows(mainRow))
        }
      }

      canvas = result

    } else throw new IllegalArgumentException(s"Invalid canvas size ${canvas.length}")
  }

  private def splitSubGrid(grid: Array[Array[Char]], subGridSize: Int): Array[Array[String]] = {

    if (grid.length % subGridSize != 0) throw new IllegalArgumentException(s"Grid is not divisible by $subGridSize")

    val size = grid.length / subGridSize
    val result: Array[Array[String]] = Array.fill(size, size)("")

    for (row <- grid.indices by subGridSize) {
      val currentRow = row / subGridSize

      for (col <- grid.indices by subGridSize) {
        val currentCol = col / subGridSize

        val patternString = (0 until subGridSize).map { rowAdd =>
          (0 until subGridSize).map { colAdd =>
            grid(row + rowAdd)(col + colAdd)
          }
        }.map(_.mkString).mkString("/")


        result(currentRow)(currentCol) = patternString
      }
    }

    result
  }

}
