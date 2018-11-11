package xyz.hyperreal.matcher


object Main extends App {

  val matcher =
    new Matchers[StringReader] {
      def input = "[[" ~> (letter*) <~ "]]"
    }

  println( matcher.input(new StringReader("[[asdf]]")) )

}