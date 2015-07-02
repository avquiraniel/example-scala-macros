package example.macros.expression

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


@compileTimeOnly("this is a macro annotation")
class CreateExpr2 extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CreateExpr2Impl.impl
}

private[macros] class CreateExpr2Impl(val c: whitebox.Context) {

  import c.universe._

  private def createExpr(t: Tree): Tree = {
    println(s"Trying to create expr from $t")
    val result = t match {
      case q"$left + $right" => q"EAdd(${createExpr(left)}, ${createExpr(right)})"
      case q"$left * $right" => q"EMul(${createExpr(left)}, ${createExpr(right)})"
      case q"$left - $right" => q"ESub(${createExpr(left)}, ${createExpr(right)})"
      case q"$left / $right" => q"EDiv(${createExpr(left)}, ${createExpr(right)})"
      case q"eval($intExpr)" => q"ENumber($intExpr)"
      case q"${number: Int}" => q"ENumber($number)"
      case q"${ident: TermName}" => q"EVar(${ident.toString})"
    }
   println(s"Result is $result")
   result
  }

  def impl(annottees: c.Tree*): c.Tree = {
    assert(annottees.tail.isEmpty)
    annottees.head match {
      case q"""$params object $name { $body }""" =>
        println(s"$name, BODY: $body")
        val expr: Tree =
          q"""$params object $name {
                import example.macros.expression._
                val expr = ${createExpr(body)}
              }
            ()"""
        expr
    }
  }
}