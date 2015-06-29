package example.macros

import org.scalatest.{FlatSpec, Matchers}

class PositionInFileTest extends FlatSpec with Matchers {
 it should "work" in {
   val position = PositionInFile.get()
   println(position)
   position.contains("PositionInFileTest") shouldBe true
 }
}
