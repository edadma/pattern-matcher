package xyz.hyperreal.matcher

import java.io.ByteArrayInputStream


object Main extends App {

  val matcher =
    new Matchers[IteratorReader] {
      def input = "[[" ~> string(rep(letter)) <~ "]]"
    }

  println( matcher.input(Reader.fromInputStream(new ByteArrayInputStream("[[asdf]]".getBytes))) )

}