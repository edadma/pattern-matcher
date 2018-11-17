package xyz.hyperreal.pattern_matcher

import org.scalatest.{Matchers => TestMatchers, _}
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with TestMatchers {
	
	"tests" in {
		new Matchers {
			delimiters += ("[[", "]]")

			def input = "[[" ~> (letter*) <~ "]]"

			input( new StringReader("[[asdf]]") ).toString shouldBe "Match(List(a, s, d, f),line 1, col 9: [[asdf]])"
		}
	}
	
}