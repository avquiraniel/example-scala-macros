# Scala macros examples
## SBT
To run tests, run
```sbt main/test```
## Examples
### CaseClassyEquals, CaseClassyToString
annotations for classes, imitating parts of case classes behaviour (still missing: apply, unapply in companion object and copy in class)
### LogMethodCalls
Each annotated method logs its arguments and return value using provided logger
### DataFrom
Reads data from file, setting type in compile time depending on file's content
### ExpressionWithDataPrinter
Prints given expression along with its value
### PositionInFile
returns file in which call is located and position in that file
### expression.CreateExpr
Transforms arithmetic expression to Expr. Doesn't really work, because by then compiler already optimised constant integer expressions (e.g. 10+5+2 becomes q"17")
### expression.CreateExpr2
Same as above, but uses annotations (but works).
