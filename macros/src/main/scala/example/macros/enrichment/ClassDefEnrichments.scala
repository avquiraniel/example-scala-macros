package example.macros.enrichment

import example.macros.Util
import monocle.Lens

import scala.reflect.macros.blackbox
trait ClassDefEnrichments[Ctx <: blackbox.Context] {
  val c: Ctx
  import c.universe._

  object ClassDefLenses {
    lazy val firstConstructorLens = defsLens composeLens Util.firstListElementLens

    lazy val bodyLens = Lens[ClassDef, List[Tree]] {
      _.impl.body
    } { newBody => classDef =>
      val ClassDef(modifiers, name, typeDefs, Template(parents, self, body)) = classDef
      ClassDef(modifiers, name, typeDefs, Template(parents, self, newBody))
    }

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

    lazy val modifiersLens = Lens[ClassDef, Modifiers] { classDef =>
      classDef.mods
    } { newMods => classDef =>
      val ClassDef(mods, name, tparams, impl) = classDef
      ClassDef(newMods, name, tparams, impl)
    }
  }


  implicit class RichClassDef(self: ClassDef) {
    def primaryConstructorParameters: List[ValDef] = {
      ClassDefLenses.defsLens.get(self).head.vparamss.head
    }

    def modifyValDefs(mod: ValDef => ValDef) = {
      val ClassDef(modifiers, name, typeDefs, Template(parents, aSelf, body)) = self
      val newBody = body.map { someDef => Util.extract[Tree, ValDef](someDef).map(mod).getOrElse(someDef) }
      ClassDef(modifiers, name, typeDefs, Template(parents, aSelf, newBody))
    }
  }
}
