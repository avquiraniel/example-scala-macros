import org.scalatest.Matchers
import pl.touk.macros.CaseClassyToString

class ToStringTest extends org.scalatest.FlatSpec with Matchers {
  it should "override toString as case classes" in {
    testSameBehaviour({
      case class X(x: Int, y: String)
      new X(42, "foo")
    }, {
      @CaseClassyToString
      class X(x: Int, y: String)
      new X(42, "foo")
    })
  }

  it should "not override toString if it exists" in {
    testSameBehaviour({
      case class X(x: Int) {
        override def toString: String = s"$x$x$x"
      }
      new X(25)
    }, {
      @CaseClassyToString
      class X(x: Int) {
        override def toString: String = s"$x$x$x"
      }
      new X(25)
    })
  }
  it should "ignore other-than-primary constructors" in {
    testSameBehaviour({
      case class X(x: Int) {
        def this(a: Int, b: Int) = this(a + b)
      }
      new X(1, 2)
    }, {
      @CaseClassyToString
      class X(x: Int) {
        def this(a: Int, b: Int) = this(a + b)
      }
      new X(1, 2)
    })
  }

  def testSameBehaviour(originalCaseClass: Any, withCaseClassyToString: Any) {
    originalCaseClass.toString shouldEqual withCaseClassyToString.toString
  }
}
