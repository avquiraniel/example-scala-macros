package example.macros.enrichment

import monocle.Lens

import scala.reflect.macros.blackbox

trait DefDefEnrichments[Ctx <: blackbox.Context] {
  val c: Ctx
  import c.universe._

  object DefDefLenses {
    lazy val argListsLens = Lens[DefDef, List[List[ValDef]]] { defDef =>
      defDef.vparamss
    } { argLists => defDef =>
      val DefDef(mods, name, tparams, vparamss, tpt, rhs) = defDef
      DefDef(mods, name, tparams, argLists, tpt, rhs)
    }
  }

  implicit class RichDefDef(self: DefDef) {
    def isConstructor: Boolean = self.name.toString == "<init>"
  }
}
