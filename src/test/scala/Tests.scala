package xyz.hyperreal.pattern_matcher

import org.scalatest.{Matchers => TestMatchers, _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import xyz.hyperreal.char_reader.CharReader


class Tests extends FreeSpec with ScalaCheckPropertyChecks with TestMatchers {
	
	"tests" in {
		new Matchers[CharReader] {
			delimiters ++= List( "[[", "]]" )

			def input = "[[" ~> (letter*) <~ "]]"

			input( CharReader.fromString("[[asdf]]") ).toString shouldBe "Match(List(a, s, d, f),<1, 9>)"
		}
	}
	
}