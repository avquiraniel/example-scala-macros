package example.macros.expression

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object CreateExpr {
  def apply(intExpr: Int): Expr = macro impl

  def impl(c: blackbox.Context)(intExpr: c.Tree): c.Tree = {
    import c.universe._
    def createExpr(t: Tree): Tree = t match {
      case q"$left + $right" => q"EAdd(${createExpr(left)}, ${createExpr(right)})"
      case q"$left * $right" => q"EMul(${createExpr(left)}, ${createExpr(right)})"
      case q"$left - $right" => q"ESub(${createExpr(left)}, ${createExpr(right)})"
      case q"$left / $right" => q"EDiv(${createExpr(left)}, ${createExpr(right)})"
      case q"${number: Int}" => q"ENumber($number)"
    }
    q"""import example.macros.expression.{ENumber, EAdd, EMul, ESub, EDiv}
       ${createExpr(intExpr)}"""
  }
}
