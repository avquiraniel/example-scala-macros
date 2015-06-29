package example.macros.enrichment

import scala.language.experimental.macros
import scala.reflect.macros.{whitebox, blackbox}

trait Fooish[T] {
  def name: String
}

object Fooish {
  implicit def provideFooish[T]: Fooish[T] = macro impl[T]

  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val typeTag = implicitly[WeakTypeTag[T]]
    val typeName = typeTag.tpe.typeSymbol.name.toString
    if (typeName.toLowerCase.contains("foo")) {
      q"""new Fooish[${TypeName(typeName)}] { val name: String = $typeName }"""
    } else {
      c.abort(c.enclosingPosition, "$typeTag is not Fooish. :(")
    }
  }
}
