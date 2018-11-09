package xyz.hyperreal.matcher


object Main extends App {

  val m =
    new Matchers[StringReader] {
      def ws = rep( space )

      def num = (rep1( digit ) <~ ws) ^^ (_.mkString.toInt)

      def expr =
        capture( "asdf", num ~ ("+" | "-")) ~ num ^^ {
          case a ~ "+" ~ b => a + b
          case a ~ "-" ~ b => a - b
        }
    }

  println( m.expr(new StringReader("123+2")) )
  println( m.substring("asdf"))

}