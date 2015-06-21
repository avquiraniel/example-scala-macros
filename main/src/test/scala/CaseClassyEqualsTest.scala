import org.scalatest.Matchers
import pl.touk.macros.pl.touk.macros.CaseClassyEquals

class CaseClassyEqualsTest extends org.scalatest.FlatSpec with Matchers {
  
  "annotated class" should "be final" in {
    // TODO find that "should not compile" function

    //@CaseClassyEquals class X(x: Int, y: String)
    //object Y extends X(1, "aaa")
  }
  
  "equals for annotated class" should
    "return true for two instances created with same constructor parameters" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo") shouldEqual new X(3, "foo")
  }

  it should "not return true for different classes" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo") should not equal "foo"
  }


}
