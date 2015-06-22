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

  object ClassDefLenses {
    lazy val firstConstructorLens = defsLens composeLens Util.firstListElementLens

    lazy val defsLens = Lens[ClassDef, List[DefDef]] { classDef =>
      classDef.impl.body.flatMap(Util.extract[Tree, DefDef](_))
    } { defDefs => classDef =>
      val ClassDef(modifiers, name, typeDefs, Template(parents, self, body)) = classDef
      val newBody = {
        val notDefDefs = body.flatMap(Util.extractNot[Tree, DefDef](_))
        (defDefs ++ notDefDefs).toList
      }
      ClassDef(modifiers, name, typeDefs, Template(parents, self, newBody))
    }

    lazy val valsLens = Lens[ClassDef, List[ValDef]] { classDef =>
      classDef.impl.body.flatMap(Util.extract[Tree, ValDef](_))
    } { valDefs => classDef =>
      val ClassDef(modifiers, name, typeDefs, Template(parents, self, body)) = classDef
      val newBody = {
        val notValDefs = body.flatMap(Util.extractNot[Tree, ValDef](_))
        (notValDefs ++ valDefs).toList
      }
      ClassDef(modifiers, name, typeDefs, Template(parents, self, newBody))
    }

    lazy val modifiersLens = Lens[ClassDef, Modifiers] { classDef =>
      classDef.mods
    } { newMods => classDef =>
      val ClassDef(mods, name, tparams, impl) = classDef
      ClassDef(newMods, name, tparams, impl)
    }
  }

  object DefDefLenses {
    lazy val argListsLens = Lens[DefDef, List[List[ValDef]]] { defDef =>
      defDef.vparamss
    } { argLists => defDef =>
      val DefDef(mods, name, tparams, vparamss, tpt, rhs) = defDef
      DefDef(mods, name, tparams, argLists, tpt, rhs)
    }
  }

  object ModifiersLenses {
    val flagsLens = Lens[Modifiers, FlagSet] { mods =>
      mods.flags
    } { newFlags => mods =>
      val Modifiers(flags, privateWithin, annotations) = mods
      Modifiers(newFlags, privateWithin, annotations)
    }
  }

  implicit class RichDefDef(self: DefDef) {
    def isConstructor: Boolean = self.name.toString == "<init>"
  }

  implicit class RichClassDef(self: ClassDef) {
    def primaryConstructorParameters: List[ValDef] = {
      ClassDefLenses.defsLens.get(self).head.vparamss.head
    }
  }
  
  implicit class RichTree(self: Tree) {
    def untypedMethodApply(name: String, params: List[Tree]): Tree =
      Apply(Select(self, TermName(name)), params)
  }

  implicit class RichFlagSet(self: FlagSet) {
    import RichFlagSet._
    def contains(other: FlagSet) = {
      (self | other) == self
    }
    def without(other: FlagSet) = {
      if(contains(other)) {
        singleFlags.filterNot(other.contains).filter(contains).reduce(_ | _)
      } else {
        self
      }
    }
  }
  object RichFlagSet {
    import Flag._
    private lazy val singleFlags = Seq(TRAIT, INTERFACE, MUTABLE, MACRO, DEFERRED, ABSTRACT, FINAL, SEALED, IMPLICIT,
      LAZY, OVERRIDE, PRIVATE, PROTECTED, LOCAL, CASE, ABSOVERRIDE, BYNAMEPARAM, PARAM, COVARIANT, CONTRAVARIANT,
      DEFAULTPARAM, PRESUPER, DEFAULTINIT, ENUM, PARAMACCESSOR, CASEACCESSOR, SYNTHETIC, ARTIFACT, STABLE)

  }
}
