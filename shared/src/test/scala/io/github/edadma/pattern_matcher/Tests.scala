package io.github.edadma.pattern_matcher

//import io.github.edadma.char_reader.CharReader
//import org.scalatest.freespec.AnyFreeSpec
//import org.scalatest.matchers.should.Matchers
//
//class Tests extends AnyFreeSpec with Matchers {
//  "tests" in {
//    new Matchers {
//      delimiters ++= Set("[[", "]]")
//
//      def input: Matcher[List[Char]] = "[[" ~> (letter *) <~ "]]"
//
//      input(CharReader.fromString("[[asdf]]")).toString shouldBe "Match(List(a, s, d, f),<1, 9, EOI>)"
//    }
//  }
//
//}
