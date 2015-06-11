package pl.touk.macros

import scala.annotation.{implicitNotFound, StaticAnnotation}


import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class LogMethodCalls extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro LogMethodCallsMacro.impl
}

private[macros] object LogMethodCallsMacro {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val enriched = annottees map {
      case q"$mods def $methodName(..$args): $retType = $body " =>
        println(methodName, args, retType, body)
        def surroundWithQuotes(str: String) = "\"" + str + "\""
        val emptyString = q""""""""
        val argNames = if(args.nonEmpty)
          args.map { case q"$_ val $argName: $_ = $_" =>
            val argNameString = argName.toString
            q"""$argNameString + "= " + $argName"""

          }.reduce { (expr, next) => q"""$expr + ", " + $next"""}
        else
          emptyString
        val methodNameString = methodName.toString
        val printlnStatement = "println(" + surroundWithQuotes(s"calling method $methodNameString(") + " + " + argNames + surroundWithQuotes(" + )") + ")"
        println(printlnStatement)
        q"""@scala.annotation.implicitNotFound("LogMethodCalls annotation needs implicit logger in scope")
            def $methodName(..$args)(implicit logger: { def log(param: Any): Unit }): $retType = {
              logger.log("Calling method " + $methodNameString + "(" + $argNames + ")")
              val result = $body
              logger.log("returning value " + result.toString)
              result
            }
        """
    }
    val inputs = annottees.toList
    println(inputs)

    Block(enriched.toList, Literal(Constant(())))
  }
}