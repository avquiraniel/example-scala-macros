package pl.touk.macros

import monocle.Lens

import scala.reflect.ClassTag

object Util {
  implicit class RichString(str: String) {
    def quote: String = "\"" + str + "\""
  }

  def firstListElementLens[A] = Lens[List[A], A] { _.head } { el => list =>
    el :: list.tail
  }



  def extract[S, T <: S : ClassTag](x: S): Option[T] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(x))
      Some(x.asInstanceOf[T])
    else
      None

  def extractNot[S, T <: S : ClassTag](x: S): Option[S] = {
    extract[S, T](x).map(_ => None).getOrElse(Some(x))
  }
}
