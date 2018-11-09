package xyz.hyperreal.matcher


object Main extends App {

  val matcher =
    new Matchers[StringReader] {
      def ws = rep( space )

      def t( s: String ) = s <~ ws

      def number = (rep1( digit ) <~ ws) ^^ (_.mkString.toInt)

      def additive: Matcher[(Int, Int) => Int] = (t("+") | t("-")) ^^ {
        case "+" => _ + _
        case "-" => _ - _
      }

      def sign: Matcher[Int => Int] = opt(("+" | "-") <~ ws) ^^ {
        case None => a => a
        case Some( "+" ) => a => a
        case Some( "-" ) => a => -a
      }

      def multiplicative: Matcher[(Int, Int) => Int] = (("*" | "/") <~ ws) ^^ {
        case "*" => _ * _
        case "/" => _ / _
      }

      def term = factor ~ rep(multiplicative ~ factor) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def factor =
        sign ~ ufactor ^^ {
          case s ~ u => s( u )
        }

      def ufactor =
        number | t("(") ~> expression <~ t(")")

      def expression: Matcher[Int] = term ~ rep(additive ~ term) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def input = expression <~ eoi
    }

  println( matcher.input(new StringReader("3 + 4 * 5")) )

}