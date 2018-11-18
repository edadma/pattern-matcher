Pattern Matcher
===============

[![Build Status](https://www.travis-ci.org/edadma/pattern-matcher.svg?branch=master)](https://www.travis-ci.org/edadma/pattern-matcher)
[![Build status](https://ci.appveyor.com/api/projects/status/h5b23n2vd0k4oh9q/branch/master?svg=true)](https://ci.appveyor.com/project/edadma/pattern-matcher/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/edadma/pattern-matcher/badge.svg?branch=master)](https://coveralls.io/github/edadma/pattern-matcher?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/pattern-matcher/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.2-orange.svg)](https://github.com/edadma/pattern-matcher/releases/tag/v0.2)

*`pattern-matcher`* is a small combinator parsing library written in Scala mainly as a learning experience. For serious parsing needs, it is recommended to use Scala's `scala-parser-combinators` library.


Example
-------

Here is an example expression parser.

```scala
import xyz.hyperreal.pattern_matcher._


object Example extends App {

  val matcher =
    new Matchers[StringReader] {
      delimiters += ("+", "-", "*", "/", "(", ")")

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

      def term = factor ~ rep(multiplicative ~ factor) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def factor = sign ~ ufactor ^^ {
        case s ~ u => s( u )
      }

      def ufactor = integerLit | "(" ~> expression <~ ")"

      def expression: Matcher[Int] = term ~ rep(additive ~ term) ^^ {
        case number ~ list => (number /: list) { case (x, f ~ y) => f( x, y ) }
      }

      def input = matchall(expression)
    }

  println( matcher.input(Reader.fromString("3 + 4 * 5")) )

}
```

### output

23


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