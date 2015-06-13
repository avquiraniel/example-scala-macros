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

  @LogMethodCalls
  def methodWithTwoParamLists(x: Int, u: String)(y: String): Boolean = {
    x.toString == y
  }

  val x = someMethod(1, "abcd")
  methodWithNoParams()
  methodWithTwoParamLists(5, "abcd")("5")
  println(x)
}
