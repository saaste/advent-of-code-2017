package helpers

object Converters {

  implicit class extendedInt(o: Int) {
    def toHex: String = {
      val hexValue = o.toHexString
      if (hexValue.length == 1) s"0$hexValue" else hexValue
    }
  }

}
