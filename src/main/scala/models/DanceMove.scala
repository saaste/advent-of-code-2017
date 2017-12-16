package models

sealed trait DanceMove
case class Spin(steps: Int) extends DanceMove
case class Exchange(a: Int, b: Int) extends DanceMove
case class Partner(a: Char, b: Char) extends DanceMove