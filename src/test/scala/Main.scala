package xyz.hyperreal.pattern_matcher


object Main extends App {

  val matcher =
    new Matchers {
      delimiters += ("[[", "]]")

      def input = "[[" ~> string(rep(letter)) <~ "]]"
    }

  println( matcher.input(Reader.fromString("[[asdf]]")) )

}