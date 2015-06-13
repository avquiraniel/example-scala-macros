package pl.touk.macros


@scala.annotation.implicitNotFound("LogMethodCalls annotation needs implicit Logger in scope")
trait Logger {
  def log(param: Any): Unit
}
