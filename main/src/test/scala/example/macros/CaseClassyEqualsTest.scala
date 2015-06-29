package example.macros

import org.scalatest.Matchers

class CaseClassyEqualsTest extends org.scalatest.FlatSpec with Matchers {
  
  "annotated class" should "be final" in {
    """| @CaseClassyEquals class X(x: Int, y: String)
       | object Y extends X(1, "aaa")""".stripMargin shouldNot compile
  }

  it should "not have vals with expressions rearranged" in {
    var testVar = 0
    @CaseClassyEquals class X(x: Int) {
      val y = 5
      testVar = y
    }
    new X(3)
    testVar shouldEqual 5
  }
  
  "equals for annotated class" should "return true for two instances of same, empty-list constructed, class" in {
    @CaseClassyEquals class Empty()

    new Empty() shouldEqual new Empty()
  }

  it should "return true for two instances created with same constructor parameters" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo") shouldEqual new X(3, "foo")
  }

  it should "not return true for classes with different values in a param" in {
    @CaseClassyEquals class X(x: Int, y: String, z: Double)

    new X(3, "foo", 2.4) should not equal new X(3, "bar", 2.4)
  }

  it should "not return true for different classes" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo") should not equal "foo"
  }

  "hashCode for annotated class" should "return same value for two instances created with same constructor parameters" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo").hashCode() shouldEqual new X(3, "foo").hashCode()
  }

  it should "return different values for two instances crated with different constructor parameters" in {
    @CaseClassyEquals class X(x: Int, y: String)

    new X(3, "foo").hashCode() should not equal new X(4, "foo").hashCode()

  }


}
