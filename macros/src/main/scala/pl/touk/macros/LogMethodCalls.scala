package pl.touk.macros

import pl.touk.macros.enrichment.Enrichments

import scala.annotation.{compileTimeOnly, implicitNotFound, StaticAnnotation}


import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("this is a macro annotation")
class LogMethodCalls extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro LogMethodCallsMacro.impl
}

private[macros] object LogMethodCallsMacro {
  import Util._

  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    val helpers = Enrichments[c.type](c)
    import c.universe._
    val enriched = annottees map {
      case q"$mods def $methodName(...$argLists): $retType = $body " =>
        println(methodName, argLists, retType, body)

        val argumentsLists = if(argLists.nonEmpty) {
          argLists.map { args =>

            val listString = if (args.nonEmpty)
              args.map { case q"$_ val $argName: $_ = $_" =>
                val argNameString = argName.toString
                q"""$argNameString + "= " + $argName"""
              }.reduce { (expr, next) => q"""$expr + ", " + $next""" }
            else
              helpers.emptyStringLiteral

            q""""(" + $listString + ")""""
          }.reduce { (lists, next) => q"""$lists + $next""" }
        } else {
          helpers.emptyStringLiteral
        }
        val methodNameString = methodName.toString
        q"""def $methodName(...$argLists): $retType = {
              val logger$$ = implicitly[Logger]
              logger$$.log("Calling method " + $methodNameString + $argumentsLists)
              val result = $body
              logger$$.log("returning value " + result.toString)
              result
            }
        """
    }
    val inputs = annottees.toList
    println(inputs)

    Block(enriched.toList, Literal(Constant(())))
  }
}