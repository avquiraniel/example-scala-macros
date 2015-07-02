package example.macros.expression

import org.scalatest.{FlatSpec, Matchers}

class CreateExpr2Test extends FlatSpec with Matchers {

  private implicit class IntToExpr(int: Int) {
    def toExpr = ENumber(int)
  }

  "Annotation expression creator" should "create a number expression" in {
    @CreateExpr2 object X {
      42
    }
    X.expr shouldEqual ENumber(42)
  }

  // Doesn't work because of Scala's compile-time integer operations
  it should "create expression with correct associativity" in {
    @CreateExpr2 object LeftAssociative {
      1 + 2 + 3
    }
    LeftAssociative.expr shouldEqual EAdd(EAdd(1.toExpr, 2.toExpr), 3.toExpr)

    @CreateExpr2 object RightAssociative {
      1 + (2 + 3)
    }
    RightAssociative.expr shouldEqual EAdd(1.toExpr, EAdd(2.toExpr, 3.toExpr))
  }

  it should "have correct operator precedence" in {
    @CreateExpr2 object X {
      1 + 2 * 3
    }
    X.expr shouldEqual EAdd(1.toExpr, EMul(2.toExpr, 3.toExpr))
  }

  it should "create expression variables from symbols" in {
    val expr = {
      @CreateExpr2 object X {
        a / b
      }
      X.expr
    }
    expr shouldEqual EDiv(EVar("a"), EVar("b"))
  }

  it should "evaluate fragments inside `eval`" in {
    val z = 35
    @CreateExpr2 object X {
      xyz - eval(z + 7)
    }
    X.expr shouldEqual ESub(EVar("xyz"), ENumber(42))
  }

  it should "fail in compile time if code inside eval is not int" in {
    """@CreateExpr2 object X {
        eval("foo")
      }""" shouldNot compile
  }

}
