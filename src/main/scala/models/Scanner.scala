package models

class Scanner() {
  private var isActive: Boolean = true
  private var range: Int = 1

  private var index: Int = -1
  private var firstZeroHit: Int = -1

  private var currentLocation: Int = 1
  private var goesDown: Boolean = true

  def move(): Unit = {
    if (isActive) {
      if (goesDown) currentLocation += 1 else currentLocation -= 1
      if (currentLocation == 1 || currentLocation == range) goesDown = !goesDown
    }
  }

  def getRange: Int = range

  def isWatching: Boolean = currentLocation == 1 && isActive

  def isHitAfterNMoves(moves: Int): Boolean = {
    if (isActive) {
      if (moves == firstZeroHit) true
      else if (moves < firstZeroHit) false
      else {
        val multiplication = (range - 1) * 2
        val secondHit = firstZeroHit + multiplication
        (moves - secondHit) % multiplication == 0
      }
    } else false
  }
}

object Scanner {

  def apply(index: Int, range: Int): Scanner = {

    if (range < 1) throw new IllegalArgumentException()

    val scanner = new Scanner()
    scanner.index = index
    scanner.range = range

    // Make initial moves by index
    (index until 0 by -1).foreach(_ => scanner.move())

    // Move until scanner current location is 1
    var counter = 0
    while(scanner.currentLocation > 1) {
      scanner.move()
      counter += 1
    }
    scanner.firstZeroHit = counter
    scanner.currentLocation = 1


    scanner
  }

  def inactive: Scanner = {
    val scanner = new Scanner()
    scanner.isActive = false
    scanner.currentLocation = 0
    scanner
  }

}
