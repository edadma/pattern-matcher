import xyz.hyperreal.pattern_matcher._

object Example1 /*extends App*/ {

  val matcher =
    new Matchers[StringReader] {
      delimiters += ("+", "-", "*", "/", "(", ")")

      def input = matchall(expression)

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

      def expression: Matcher[Int] = sign ~ term ~ rep(additive ~ term) ^^ {
        case s ~ n ~ l => (s(n) /: l) { case (x, f ~ y) => f( x, y ) }
      }

      def term = factor ~ rep(multiplicative ~ factor) ^^ {
        case n ~ l => (n /: l) { case (x, f ~ y) => f( x, y ) }
      }

      def factor = integerLit | "(" ~> expression <~ ")"
    }

  def run( s: String ) =
    matcher.input( Reader.fromString(s) ) match {
      case matcher.Match( result, _ ) => println( result )
      case m: matcher.Mismatch => m.print
    }

  run( "-3 + 4 * (-5)" )
  run( "5" )
  run( "2 +" )

}