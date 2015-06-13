package pl.touk.macros

import scala.reflect.macros.blackbox

case class MacroHelpers[Ctx <: blackbox.Context](c: Ctx) {
  import c.universe._
  def valueToLiteral(value: String): c.universe.Tree = {
    println(q"""$value""")
    q"""$value"""
  }

  def emptyStringLiteral: c.universe.Tree = valueToLiteral("")

  def join(a: c.universe.Tree, b: c.universe.Tree): c.universe.Tree = {
    q"""$a + $b"""
  }
}
