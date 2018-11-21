Pattern Matcher
===============

[![Build Status](https://www.travis-ci.org/edadma/pattern-matcher.svg?branch=master)](https://www.travis-ci.org/edadma/pattern-matcher)
[![Build status](https://ci.appveyor.com/api/projects/status/h5b23n2vd0k4oh9q/branch/master?svg=true)](https://ci.appveyor.com/project/edadma/pattern-matcher/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/edadma/pattern-matcher/badge.svg?branch=master)](https://coveralls.io/github/edadma/pattern-matcher?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/pattern-matcher/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.2-orange.svg)](https://github.com/edadma/pattern-matcher/releases/tag/v0.2)

*`pattern-matcher`* is a small combinator parsing library written in Scala mainly as a learning experience. For serious parsing needs, it is recommended to use Scala's `scala-parser-combinators` library.


Example 1
---------

Here is an example expression parser.

```scala
import xyz.hyperreal.pattern_matcher._

object Example1 extends App {

  val matcher =
    new Matchers[StringReader] {
      delimiters += ("+", "-", "*", "/", "(", ")")

      def input = matchall(expression)

      def additive: Matcher[(Int, Int) => Int] = ("+" | "-") ^^ {
        case "+" => _ + _
        case "-" => _ - _
      }

      def sign: Matcher[Int => Int] = opt("+" | "-") ^^ {
        case Some( "-" ) => -_
        case _ => a => a
      }

      def multiplicative: Matcher[(Int, Int) => Int] = ("*" | "/") ^^ {
        case "*" => _ * _
        case "/" => _ / _
      }

      def expression: Matcher[Int] = sign ~ term ~ rep(additive ~ term) ^^ {
        case s ~ n ~ l => (s(n) /: l) { case (x, f ~ y) => f( x, y ) }
      }

      def term = factor ~ rep(multiplicative ~ factor) ^^ {
        case n ~ l => (n /: l) { case (x, f ~ y) => f( x, y ) }
      }

      def factor = integerLit | "(" ~> expression <~ ")"
    }

  def run( s: String ) =
    matcher.input( Reader.fromString(s) ) match {
      case matcher.Match( result, _ ) => println( result )
      case m: matcher.Mismatch => m.print
    }

  run( "-3 + 4 * 5" )
  run( "-5" )
  run( "2 +" )

}
```

### output

-23
5
expected end of input (line 1, column 3):
2 +


Example 2
---------

As a longer example, here is an implementation of Niklaus Wirth's PL/0 programming language from his 1976 book "Algorithms + Data Structures = Programs".

```scala
```


Usage
-----

### Library

Use the following definition to use *pattern-matcher* in your Maven project:

```xml
<repository>
  <id>hyperreal</id>
  <url>https://dl.bintray.com/edadma/maven</url>
</repository>

<dependency>
  <groupId>xyz.hyperreal</groupId>
  <artifactId>pattern-matcher</artifactId>
  <version>0.2</version>
</dependency>
```

Add the following to your `build.sbt` file to use Backslash in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "pattern-matcher" % "0.2"
```

Building
--------

### Requirements

- Java 11+
- SBT 1.2.6+
- Scala 2.12.7+

### Clone and Assemble Executable

```bash
git clone git://github.com/edadma/pattern-matcher.git
cd pattern-matcher
sbt assembly
```

The command `sbt assembly` also runs all the unit tests.


License
-------

ISC © 2018 Edward A. Maxedon, Sr.