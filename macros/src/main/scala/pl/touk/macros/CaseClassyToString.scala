package pl.touk.macros

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("this is a macro annotation")
class CaseClassyToString extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassyToString.impl
}

object CaseClassyToString {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    val helpers = MacroHelpers[c.type](c)
    import c.universe._

    println(s"CaseClassyToStringAnnottees: $annottees")
    val enriched = annottees.head match {
      case classDef: ClassDef =>
        val withNewToString = helpers.modClassDefs(classDef) { defDefs =>
          defDefs.flatMap {
            case method@DefDef(modifiers, termName, genericTypes, argLists, tr, tr2) if helpers.isConstructor(method) =>
              println(s"modifiers: $modifiers, termName: $termName, genericTypes: $genericTypes, argLists: $argLists, tr: $tr, tr2: $tr2")
              val firstArgList = argLists.headOption.getOrElse(Nil)
              def concat(a: Tree, b: Tree): Tree = Apply(Select(a, TermName("concat")), b :: Nil)
              val toStringBody = {
                val start = Literal(Constant(classDef.name.toString + "("))
                val end = Literal(Constant(")"))
                val stringsToConcat = firstArgList.map(d => Select(Ident(d.name), TermName("toString")))
                val printedArgs = Apply(Select(Apply(Ident(TermName("List")), stringsToConcat), TermName("mkString")), Literal(Constant(",")) :: Nil)
                concat(start, concat(printedArgs, end))
              }
              val toStringMethod = DefDef(Modifiers(Flag.OVERRIDE), TermName("toString"), Nil, Nil, Ident(TypeName("String")), toStringBody)
              Seq(method, toStringMethod)
            case toStringMethod@DefDef(_, termName, _, _, _, _) if termName.toString == "toString" =>
              Nil
            case other =>
              Seq(other)
          }
        }
        println(s"new to String: $withNewToString")
        withNewToString +: annottees.tail
      case _ =>
        println("Not classdef")
        annottees
    }
    Block(enriched.toList, Literal(Constant(())))
  }
}