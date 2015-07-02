package example.macros.expression

sealed trait Expr
case class ENumber(value: Int) extends Expr
case class EAdd(left: Expr, right: Expr) extends Expr
case class EMul(left: Expr, right: Expr) extends Expr
case class ESub(left: Expr, right: Expr) extends Expr
case class EDiv(left: Expr, right: Expr) extends Expr
case class EVar(name: String) extends Expr


