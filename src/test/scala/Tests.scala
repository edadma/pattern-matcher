package xyz.hyperreal.pattern_matcher

import org.scalatest.{Matchers => TestMatchers, _}
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with TestMatchers {
	
	"tests" in {
		new Matchers[StringReader] {
			delimiters += ("[[", "]]")

			def input = "[[" ~> (letter*) <~ "]]"

			input( Reader.fromString("[[asdf]]") ).toString shouldBe "Match(List(a, s, d, f),<1, 9>)"
		}
	}
	
}