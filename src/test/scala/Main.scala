package xyz.hyperreal.matcher


object Main extends App {

  val matcher =
    new Matchers[StringReader] {

      def input = '[' ~> matchedString(rep(noneOf(']'))) <~ ']'
    }

  println( matcher.input(new StringReader("[asdf]")) )

}