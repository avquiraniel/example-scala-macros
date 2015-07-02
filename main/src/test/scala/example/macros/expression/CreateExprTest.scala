package example.macros.expression

import org.scalatest.{Matchers, FlatSpec}

class CreateExprTest extends FlatSpec with Matchers {
  import CreateExpr._
  private implicit class IntToExpr(int: Int) {
    def toExpr = ENumber(int)
  }

  "Expression creator" should "create a number expression" in {
    CreateExpr(42) shouldEqual ENumber(42)
  }

  // Doesn't work because of Scalas compile-time integer operations
//  it should "create expression with correct associativity" in {
//    CreateExpr(1+2+3) shouldEqual EAdd(EAdd(1.toExpr, 2.toExpr), 3.toExpr)
//    CreateExpr(1+(2+3)) shouldEqual EAdd(1.toExpr, EAdd(2.toExpr, 3.toExpr))
//  }
}
