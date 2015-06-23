package pl.touk.macros.enrichment

import monocle.Lens
import pl.touk.macros.Util

import scala.reflect.macros.blackbox

trait FlagSetEnrichments[Ctx <: blackbox.Context] {
  val c: Ctx
  import c.universe._

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
