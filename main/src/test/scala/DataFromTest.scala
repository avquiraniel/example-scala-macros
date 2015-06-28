import org.scalatest.{Matchers, FlatSpec}
import pl.touk.macros.DataFrom

class DataFromTest extends FlatSpec with Matchers {
  "DataFrom" should "return string from string file" in {
    val result = DataFrom("../resources/DataFromTest/stringFile.txt")
    result shouldEqual "foo"
  }

  it should "have proper return type" in {
    val result: String = DataFrom("../resources/DataFromTest/stringFile.txt")
    """val result: Int = DataFrom("../resources/DataFromTest/stringFile.txt")""" shouldNot compile
  }

  it should "return Int from int file" in {
    val result: Int = DataFrom("../resources/DataFromTest/intFile.txt")
    result shouldEqual 42
  }

  it should "return Double from double file" in {
    val result: Double = DataFrom("../resources/DataFromTest/doubleFile.txt")
    result shouldEqual 3.14
  }
}
