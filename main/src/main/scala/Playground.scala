import pl.touk.macros.{Logger, LogMethodCalls}

object Playground extends App {
  implicit val logger = new Logger {
     def log(param: Any): Unit = {
       println(param)
     }
  }

  @LogMethodCalls
  def someMethod(num: Int, blah: String): String = {
    methodWithNoParams()
    s"$blah-$num"
  }

  @LogMethodCalls
  def methodWithNoParams() = {
    5
  }

  val x = someMethod(1, "abcd")
  methodWithNoParams()
  println(x)
}
