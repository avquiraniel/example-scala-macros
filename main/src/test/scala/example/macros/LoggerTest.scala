package example.macros

import org.scalatest.{FlatSpec, Matchers}

import scala.util.control.NonFatal

class LoggerTest extends FlatSpec with Matchers {
  class LogToString extends Logger {
    private var logs: Seq[String] = IndexedSeq.empty
    def log(param: Any): Unit = {
      logs = logs :+ param.toString
    }
    def showLogs = logs
  }

  "Annotated defs" should "log their parameters" in {
    implicit val logger = new LogToString
    object Obj {
      @LogMethodCalls
      def method(x: String, y: Int): Unit = {}
    }
    Obj.method("foo", 42)
    println(logger.showLogs)
    logger.showLogs.mkString.contains("x= foo, y= 42") shouldBe true
  }

  it should "log parametes even if call is interrupted" in {
    implicit val logger = new LogToString
    object Obj {
      @LogMethodCalls
      def method(x: String, y: Int): Unit = ???
    }
    try {
      Obj.method("foo", 42)
    } catch { case NonFatal(_) => }
    logger.showLogs should not be empty
  }

  it should "log its return value" in {
    implicit val logger = new LogToString
    object Obj {
      @LogMethodCalls
      def method(x: String, y: Int): String = x + y.toString
    }
    val result = Obj.method("foo", 42)
    result shouldEqual "foo42"
    logger.showLogs(1).contains(result) shouldBe true
  }
}
