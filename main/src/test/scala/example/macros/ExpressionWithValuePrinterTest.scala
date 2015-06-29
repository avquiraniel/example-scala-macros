package example.macros

import org.scalatest.{FlatSpec, Matchers}

class ExpressionWithValuePrinterTest extends FlatSpec with Matchers {
  it should "print expression with its value" in {
    val x = 2
    ExpressionWithValuePrinter.print(x + x) shouldEqual "Expression x.+(x) has value 4"
  }
}
