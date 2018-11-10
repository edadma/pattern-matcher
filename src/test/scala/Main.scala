import xyz.hyperreal.matcher._


object Example extends App {

  val matcher =
    new Matchers[StringReader] {
      def ws = rep(space)

      def t[S]( m: => Matcher[S] ) = m <~ ws

      def number = t(rep1(digit)) ^^ (_.mkString.toInt)

      def additive: Matcher[(Int, Int) => Int] = t("+" | "-") ^^ {
        case "+" => _ + _
        case "-" => _ - _
      }

      def sign: Matcher[Int => Int] = opt(t("+" | "-")) ^^ {
        case None | Some( "+" ) => a => a
        case Some( "-" ) => a => -a
      }

      def multiplicative: Matcher[(Int, Int) => Int] = t("*" | "/") ^^ {
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

      def input = ws ~> expression <~ eoi
    }

  println( matcher.input(new StringReader("3 + 4 * 5")) )

}