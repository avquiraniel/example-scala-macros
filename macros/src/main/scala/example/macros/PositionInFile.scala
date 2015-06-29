package example.macros

import scala.language.experimental.macros

object PositionInFile {
  def get(): String = macro PositionInFileImpl.print
}

private[macros] class PositionInFileImpl(val c: scala.reflect.macros.whitebox.Context) {
  import c.universe._

  def print(): c.Tree = {
    Literal(Constant(c.enclosingPosition.toString))
  }
}
