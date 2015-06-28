package pl.touk.macros

import java.io.File

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object DataFrom {
  def apply(locationOnDisk: String): Any = macro DataFromImpl.impl
}

private[macros] class DataFromImpl(val c: whitebox.Context) {
  import c.universe._

  def impl(locationOnDisk: c.Tree): c.Tree = locationOnDisk match {
    case Literal(Constant(location: String))  =>
      val position = c.enclosingPosition.source.file.file.getParentFile
      val file = new File(position, location)
      val content = scala.io.Source.fromFile(file).mkString
      val (typeString, contentString) = (content.split(":").head, content.split(":").tail.mkString(":"))
      Literal(Constant(typeString match {
        case "String" => contentString
        case "Int"    => contentString.toInt
        case "Double" => contentString.toDouble
        case _        => c.abort(c.enclosingPosition, s"Unsupported data type")
      }))
    case _ => c.abort(c.enclosingPosition, s"A string literal should be passed to $DataFrom")
  }
}