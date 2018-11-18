Pattern Matcher
===============

*`pattern-matcher`* is a small combinator parsing library written in Scala mainly as a learning experience. For serious parsing needs, it is recommended to use Scala's `scala-parser-combinators` library.


Example
-------

Here is an example expression parser.

```scala
import xyz.hyperreal.pattern_matcher._


object Example extends App {

  val matcher =
    new Matchers[StringReader] {
      delimiters += ("+", "-", "*", "/", "(", ")")

      def additive: Matcher[(Int, Int) => Int] = ("+" | "-") ^^ {
        case "+" => _ + _
        case "-" => _ - _
      }

      def sign: Matcher[Int => Int] = opt("+" | "-") ^^ {
        case Some( "-" ) => -_
        case _ => a => a
      }

      def multiplicative: Matcher[(Int, Int) => Int] = ("*" | "/") ^^ {
        case "*" => _ * _
        case "/" => _ / _
      }

      def term = factor ~ rep(multiplicative ~ factor) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def factor = sign ~ ufactor ^^ {
        case s ~ u => s( u )
      }

      def ufactor = integerLit | "(" ~> expression <~ ")"

      def expression: Matcher[Int] = term ~ rep(additive ~ term) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def input = matchall(expression)
    }

  println( matcher.input(Reader.fromString("3 + 4 * 5")) )

}
```

### output

23