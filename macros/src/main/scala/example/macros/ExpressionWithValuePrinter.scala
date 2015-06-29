package example.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object ExpressionWithValuePrinter {

  def print(expr: Any): String = macro impl

  def impl(c: blackbox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._
    val exprAsString =  expr.toString
    q""""Expression " + $exprAsString + " has value " + $expr.toString"""
  }
}
