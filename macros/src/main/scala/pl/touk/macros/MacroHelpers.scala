package pl.touk.macros

import monocle.Lens

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

case class MacroHelpers[Ctx <: blackbox.Context](c: Ctx) {
  import c.universe._
  def valueToLiteral(value: String): Tree = {
    Literal(Constant(value))
  }

  def emptyStringLiteral: Tree = valueToLiteral("")

  def join(a: Tree, b: Tree): Tree = {
    q"""$a + $b"""
  }

  def extract[S, T <: S : ClassTag](x: S): Option[T] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(x))
      Some(x.asInstanceOf[T])
    else
      None

  def extractNot[S, T <: S : ClassTag](x: S): Option[S] = {
    extract[S, T](x).map(_ => None).getOrElse(Some(x))
  }

  object ClassDefLenses {
    val defsLens = Lens[ClassDef, List[DefDef]] { classDef =>
      classDef.impl.body.flatMap(extract[Tree, DefDef](_))
    } { defDefs => classDef =>
      val ClassDef(modifiers, name, typeDefs, Template(parents, self, body)) = classDef
      val newBody = {
        val notDefDefs = body.flatMap(extractNot[Tree, DefDef](_))
        (defDefs ++ notDefDefs).toList
      }
      ClassDef(modifiers, name, typeDefs, Template(parents, self, newBody))
    }
  }

  def isConstructor(defDef: DefDef): Boolean = defDef.name == "<init>"
}
