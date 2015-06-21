package pl.touk.macros

package pl.touk.macros

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("this is a macro annotation")
class CaseClassyEquals extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassyEqualsImpl.impl
}

private[macros] class CaseClassyEqualsImpl(val c: whitebox.Context) {
  import c.universe._
  val helpers = MacroHelpers[c.type](c)
  import helpers._

  def generateEqualsMethod(classDef: ClassDef) = {
    val equalsBody: Tree = {
      val otherCanEqualThis = Block(
        ValDef(Modifiers(), TermName("otherTyped"), Ident(classDef.name),
          TypeApply(Select(Ident(TermName("other")), TermName("asInstanceOf")), Ident(classDef.name) :: Nil)
        ) :: Nil,
        Apply(Select(Ident(TermName("otherTyped")), TermName("canEqual")), This(classDef.name) :: Nil))
      If(TypeApply(Select(Ident(TermName("other")), TermName("isInstanceOf")), Ident(classDef.name) :: Nil), otherCanEqualThis, reify(false).tree)
    }
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      TermName("equals"),
      Nil,
      (ValDef(Modifiers(), TermName("other"), Ident(TypeName("Any")), EmptyTree) :: Nil) :: Nil,
      Ident(TypeName("Boolean")),
      equalsBody
    )
  }

  def enrichDefDefs(defDefs: List[DefDef], classDef: ClassDef): List[DefDef] = {
    if(defDefs.exists(_.name.toString == "equals"))
      defDefs
    else {
      val canEqualDef = generateCanEqualMethod(classDef.name)
      val equalsDef = generateEqualsMethod(classDef)
      defDefs :+ canEqualDef :+ equalsDef
    }
  }

  def generateCanEqualMethod(className: TypeName): c.universe.DefDef = {
    DefDef(
      Modifiers(Flag.SYNTHETIC),
      TermName("canEqual"),
      Nil,
      (ValDef(Modifiers(), TermName("other"), Ident(TypeName("Any")), EmptyTree) :: Nil) :: Nil,
      Ident(TypeName("Boolean")),
      TypeApply(Select(Ident(TermName("other")), TermName("isInstanceOf")), Ident(className) :: Nil)
    )
  }

  def impl(annottees: c.Tree*): c.Tree = {
    val enriched = annottees.head match {
      case classDef: ClassDef =>
        classDef.impl.body.foreach {

          case ValDef(_, _, _, rhs) => println(s"rhs: $rhs")
          case _ => ()
        }

        val finalized = (ClassDefLenses.modifiersLens composeLens ModifiersLenses.flagsLens).modify { flags =>
          flags | Flag.FINAL
        } (classDef)

        val withNewEquals = ClassDefLenses.defsLens.modify { defDefs =>
          enrichDefDefs(defDefs, classDef)
        }(finalized)
        withNewEquals +: annottees.tail
      case _ =>
        annottees
    }

    Block(enriched.toList, Literal(Constant(())))
  }
}