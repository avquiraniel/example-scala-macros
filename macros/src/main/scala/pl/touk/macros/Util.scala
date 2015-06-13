package pl.touk.macros

object Util {
  implicit class RichString(str: String) {
    def quote: String = "\"" + str + "\""
  }
}
