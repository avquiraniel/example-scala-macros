package example.macros

import example.macros.enrichment.Fooish
import org.scalatest.{FlatSpec, Matchers}

class FooishTest extends FlatSpec with Matchers {
  "Fooish" should "be found for Foo class" in {
    class Foo
    val fooish = implicitly[Fooish[Foo]]
    fooish.name shouldEqual "Foo"
  }

  it should "not be found for Bar class" in {
    """| class Bar
       | val fooish = implicitly[Fooish[Bar]]""".stripMargin shouldNot compile
  }
}
