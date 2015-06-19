package pl.touk.macros

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("this is a macro annotation")
class CaseClassyToString extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassyToStringImpl.impl
}

private[macros] class CaseClassyToStringImpl(val c: whitebox.Context) {
  import c.universe._
  val helpers = MacroHelpers[c.type](c)
  import helpers._
  def impl(annottees: c.Tree*): c.Tree = {
    println("called CaseClassyToString")
    val enriched = annottees.head match {
      case classDef: ClassDef =>
        val withNewToString = ClassDefLenses.defsLens.modify { defDefs =>
          enrichDefDefs(defDefs, classDef.name)
        }(classDef)
        withNewToString +: annottees.tail
      case _ =>
        annottees
    }

    Block(enriched.toList, Literal(Constant(())))
  }

  private def enrichDefDefs(defDefs: List[DefDef], className: TypeName): List[DefDef] = {
    if (defDefs.exists(_.name.toString == "toString")) {
      defDefs
    } else {
      val primaryConstructor = defDefs.filter(_.isConstructor).head
      val firstArgList = primaryConstructor.vparamss.headOption.getOrElse(Nil)
      val toStringMethod: DefDef = generateToStringMethod(className, firstArgList)
      println(toStringMethod)
      defDefs :+ toStringMethod
    }
  }

  def generateToStringMethod(className: TypeName, firstArgList: List[c.universe.ValDef]): DefDef = {
    val toStringBody = {
      val start = Literal(Constant(className.toString + "("))
      val end = Literal(Constant(")"))
      val stingArgs = firstArgList.map(d => Select(Ident(d.name), TermName("toString")))
      val printedArgs = Apply(Select(Apply(Ident(TermName("List")), stingArgs), TermName("mkString")), Literal(Constant(",")) :: Nil)
      concat(start, concat(printedArgs, end))
    }
    DefDef(Modifiers(Flag.OVERRIDE), TermName("toString"), Nil, Nil, Ident(TypeName("String")), toStringBody)
  }

  def concat(a: Tree, b: Tree): Tree = Apply(Select(a, TermName("concat")), b :: Nil)
}