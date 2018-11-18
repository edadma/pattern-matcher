package xyz.hyperreal.pattern_matcher


object Main extends App {

  val matcher =
    new Matchers[StringReader] {
      delimiters += ("[[", "]]")

      def input = matchall("[[" ~> string(rep(letter)) <~ "]]")
    }

  println( matcher.input(Reader.fromString("[[asdf]]")) )

}