package io.github.edadma.pattern_matcher

import io.github.edadma.char_reader.CharReader
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should

class Tests extends AnyFreeSpec with should.Matchers {

  "tests" in {
    new Matchers[CharReader] {
      delimiters ++= List("[[", "]]")

      def input: Matcher[List[Char]] = "[[" ~> (letter *) <~ "]]"

      input(CharReader.fromString("[[asdf]]")).toString shouldBe "Match(List(a, s, d, f),<1, 9, EOI>)"
    }
  }

}
