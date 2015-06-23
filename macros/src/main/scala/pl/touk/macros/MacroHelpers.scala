package pl.touk.macros

import monocle.Lens

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

trait MacroHelpers[Ctx <: blackbox.Context] {
  val c: Ctx
  import c.universe._
  def valueToLiteral(value: String): Tree = {
    Literal(Constant(value))
  }

  def emptyStringLiteral: Tree = valueToLiteral("")

  def join(a: Tree, b: Tree): Tree = {
    q"""$a + $b"""
  }

  object ModifiersLenses {
    val flagsLens = Lens[Modifiers, FlagSet] { mods =>
      mods.flags
    } { newFlags => mods =>
      val Modifiers(flags, privateWithin, annotations) = mods
      Modifiers(newFlags, privateWithin, annotations)
    }
  }

  implicit class RichTree(self: Tree) {
    def untypedMethodApply(name: String, params: List[Tree]): Tree =
      Apply(Select(self, TermName(name)), params)
  }
}
