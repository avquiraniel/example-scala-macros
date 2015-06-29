package example.macros.enrichment

import example.macros.MacroHelpers

import scala.reflect.macros.blackbox

case class Enrichments[Ctx <: blackbox.Context](c: Ctx) extends
  ClassDefEnrichments[Ctx] with
  DefDefEnrichments[Ctx] with
  FlagSetEnrichments[Ctx] with
  MacroHelpers[Ctx]
