package xyz.hyperreal.pattern_matcher

import org.scalatest.{Matchers => TestMatchers, _}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Tests extends FreeSpec with ScalaCheckPropertyChecks with TestMatchers {
	
	"tests" in {
		new Matchers[StringReader] {
			delimiters ++= List( "[[", "]]" )

			def input = "[[" ~> (letter*) <~ "]]"

			input( Reader.fromString("[[asdf]]") ).toString shouldBe "Match(List(a, s, d, f),<1, 9>)"
		}
	}
	
}