package example.macros

import org.scalatest.{FlatSpec, Matchers}

class DataFromTest extends FlatSpec with Matchers {
  "DataFrom" should "return string from string file" in {
    val result = DataFrom("../../../resources/example/macros/stringFile.txt")
    result shouldEqual "foo"
  }

  it should "have proper return type" in {
    val result: String = DataFrom("../../../resources/example/macros/stringFile.txt")
    """val result: Int = DataFrom("../../../resources/example/macros/stringFile.txt")""" shouldNot compile
  }

  it should "return Int from int file" in {
    val result: Int = DataFrom("../../../resources/example/macros/intFile.txt")
    result shouldEqual 42
  }

  it should "return Double from double file" in {
    val result: Double = DataFrom("../../../resources/example/macros/doubleFile.txt")
    result shouldEqual 3.14
  }
}
