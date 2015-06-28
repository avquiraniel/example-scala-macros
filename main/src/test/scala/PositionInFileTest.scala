import org.scalatest.{FlatSpec, Matchers}
import pl.touk.macros.PositionInFile

class PositionInFileTest extends FlatSpec with Matchers {
 it should "work" in {
   val position = PositionInFile.get()
   println(position)
   position.contains("PositionInFileTest") shouldBe true
 }
}
