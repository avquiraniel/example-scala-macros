package pl.touk.macros

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

case class MacroHelpers[Ctx <: blackbox.Context](c: Ctx) {
  import c.universe._
  def valueToLiteral(value: String): c.universe.Tree = {
    println(q"""$value""")
    q"""$value"""
  }

  def emptyStringLiteral: c.universe.Tree = valueToLiteral("")

  def join(a: c.universe.Tree, b: c.universe.Tree): c.universe.Tree = {
    q"""$a + $b"""
  }

  def modClassDefs(classDef: ClassDef)(changeDefDefs: Seq[DefDef] => Seq[DefDef]): ClassDef = {
    val ClassDef(modifiers, name, typeDefs, template) = classDef
    val Template(parents, self, body) = template
    val newBody = {
      val defDefs = body.collect { case x:DefDef => x }
      val others = body.collect { case x if !implicitly[ClassTag[DefDef]].runtimeClass.isInstance(x) => x }
      (changeDefDefs(defDefs) ++ others).toList
    }
    val newTemplate = Template(parents, self, newBody)
    ClassDef(modifiers, name, typeDefs, newTemplate)
  }

  def isConstructor(defDef: DefDef): Boolean = defDef match {
    case DefDef(_, termName, _, _, _, _) if termName.toString == "<init>" => true
    case _ => false
  }
}
