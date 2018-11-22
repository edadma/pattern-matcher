Pattern Matcher
===============

[![Build Status](https://www.travis-ci.org/edadma/pattern-matcher.svg?branch=master)](https://www.travis-ci.org/edadma/pattern-matcher)
[![Build status](https://ci.appveyor.com/api/projects/status/h5b23n2vd0k4oh9q/branch/master?svg=true)](https://ci.appveyor.com/project/edadma/pattern-matcher/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/edadma/pattern-matcher/badge.svg?branch=master)](https://coveralls.io/github/edadma/pattern-matcher?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/pattern-matcher/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.2.1-orange.svg)](https://github.com/edadma/pattern-matcher/releases/tag/v0.2.1)

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

As a longer example, here is an implementation (in under 170 lines of code) of Niklaus Wirth's PL/0 programming language from his 1976 book "Algorithms + Data Structures = Programs".

```scala
import xyz.hyperreal.pattern_matcher._

object Example2 extends App {

  val matcher =
    new Matchers[StringReader] {
      reserved += ("const", "var", "procedure", "odd", "begin", "end", "if", "then", "while", "do", "call", "write")
      delimiters += ("+", "-", "*", "/", "(", ")", ";", ",", ".", ":=", "=", "#", "<", "<=", ">", ">=")

      def program = matchall(block <~ ".")

      def const = ident ~ "=" ~ integerLit ^^ {
        case n ~ _ ~ v => Constant( n, v )
      }

      def consts = opt("const" ~> rep1sep(const, ",") <~ ";") ^^ {
        case None => Nil
        case Some( l ) => l
      }

      def vari = ident ~ opt("=" ~> integerLit) ^^ {
        case n ~ None => Variable( n, 0 )
        case n ~ Some( v ) => Variable( n, v )
      }

      def vars = opt("var" ~> rep1sep(vari, ",") <~ ";") ^^ {
        case None => Nil
        case Some( l ) => l
      }

      def proc: Matcher[Procedure] = "procedure" ~ ident ~ ";" ~ block ~ ";" ^^ {
        case _ ~ n ~ _ ~ b ~ _ => Procedure( n, b )
      }

      def block = consts ~ vars ~ rep(proc) ~ statement ^^ {
        case c ~ v ~ p ~ s => Block( c ++ v ++ p, s )
      }

      def statement: Matcher[Statement] =
        pos ~ ident ~ ":=" ~ expression ^^ { case p ~ n ~ _ ~ e => Assign( p, n, e ) } |
        "call" ~ pos ~ ident ^^ { case _ ~ p ~ n => Call( p, n ) } |
        "write" ~> expression ^^ Write |
        "begin" ~> rep1sep(statement, ";") <~ "end" ^^ Sequence |
        "if" ~ condition ~ "then" ~ statement ^^ { case _ ~ c ~ _ ~ s => If( c, s ) } |
        "while" ~ condition ~ "do" ~ statement ^^ { case _ ~ c ~ _ ~ s => While( c, s ) }

      def condition =
        "odd" ~> expression ^^ Odd |
        expression ~ ("="|"#"|"<"|"<="|">"|">=") ~ expression ^^ { case l ~ c ~ r => Comparison( l, c, r ) }

      def expression: Matcher[Expression] = opt("+" | "-") ~ term ~ rep(("+" | "-") ~ term) ^^ {
        case (None|Some("+")) ~ t ~ l => (t /: l) { case (x, o ~ y) => Operation( x, o, y ) }
        case _ ~ t ~ l => ((Negate( t ): Expression) /: l) { case (x, o ~ y) => Operation( x, o, y ) }
      }

      def term = factor ~ rep(("*" | "/") ~ factor) ^^ {
        case f ~ l => (f /: l) { case (x, o ~ y) => Operation( x, o, y ) }
      }

      def factor =
        pos ~ ident ^^ { case p ~ v => Ident( p, v ) } |
        integerLit ^^ Number |
        "(" ~> expression <~ ")"
    }

  def run( s: String ) =
    matcher.program( Reader.fromString(s) ) match {
      case matcher.Match( result, _ ) => evalBlock( result, Nil )
      case m: matcher.Mismatch => m.print
    }

  def evalBlock( block: Block, outer: List[Map[String, Any]] ): Unit = {
    val scope =
      block.decls.map {
        case Constant( name, value ) => name -> value
        case Variable( name, init ) => name -> new Var( init )
        case Procedure( name, body ) => name -> body
      }.toMap :: outer

    evalStatement( block.stat, scope )
  }

  def find( name: String, scope: List[Map[String, Any]] ): Option[(Any, List[Map[String, Any]])] =
    scope match {
      case Nil => None
      case inner@h :: t => h get name match {
        case None => find( name, t )
        case Some( v ) => Some( (v, inner) )
      }
    }

  def evalStatement( stat: Statement, scope: List[Map[String, Any]] ): Unit =
    stat match {
      case Assign( pos, name, expr ) =>
        find( name, scope ) match {
          case None => sys.error( pos.longErrorText(s"variable '$name' not declared") )
          case Some( (v: Var, _) ) => v.v = evalExpression( expr, scope )
          case _ => sys.error( pos.longErrorText(s"'$name' not assignable") )
        }
      case Call( pos, name ) =>
        find( name, scope ) match {
          case None => sys.error( pos.longErrorText(s"procedure '$name' not declared") )
          case Some( (b: Block, inner) ) => evalBlock( b, inner )
          case _ => sys.error( pos.longErrorText(s"'$name' not a procedure") )
        }

      case Write( expr ) => println( evalExpression(expr, scope) )
      case Sequence( stats ) => stats foreach (evalStatement( _, scope ))
      case If( cond, body ) => if (evalCondition( cond, scope )) evalStatement( body, scope )
      case While( cond, body ) => while (evalCondition( cond, scope )) evalStatement( body, scope )
    }

  def evalCondition( cond: Condition, scope: List[Map[String, Any]] ) =
    cond match {
      case Odd( expr ) => evalExpression( expr, scope )%2 == 1
      case Comparison( left, "<", right ) => evalExpression( left, scope ) < evalExpression( right, scope )
      case Comparison( left, ">", right ) => evalExpression( left, scope ) > evalExpression( right, scope )
      case Comparison( left, "=", right ) => evalExpression( left, scope ) == evalExpression( right, scope )
      case Comparison( left, "#", right ) => evalExpression( left, scope ) != evalExpression( right, scope )
      case Comparison( left, "<=", right ) => evalExpression( left, scope ) <= evalExpression( right, scope )
      case Comparison( left, ">=", right ) => evalExpression( left, scope ) >= evalExpression( right, scope )
    }

  def evalExpression( expr: Expression, scope: List[Map[String, Any]] ): Int =
    expr match {
      case Ident( pos, name ) =>
        find( name, scope ) match {
          case None => sys.error( pos.longErrorText(s"'$name' not declared") )
          case Some( (v: Var, _) ) => v.v
          case Some( (n: Int, _) ) => n
          case _ => sys.error( pos.longErrorText(s"'$name' not an integer") )
        }
      case Number( n ) => n
      case Operation( left, "+", right ) => evalExpression( left, scope ) + evalExpression( right, scope )
      case Operation( left, "-", right ) => evalExpression( left, scope ) - evalExpression( right, scope )
      case Operation( left, "*", right ) => evalExpression( left, scope ) * evalExpression( right, scope )
      case Operation( left, "/", right ) => evalExpression( left, scope ) / evalExpression( right, scope )
    }

  class Var( var v: Int )

  case class Block( decls: List[Declaration], stat: Statement )

  abstract class Declaration
  case class Constant( name: String, value: Int ) extends Declaration
  case class Variable( name: String, init: Int ) extends Declaration
  case class Procedure( name: String, body: Block ) extends Declaration

  abstract class Statement
  case class Assign( pos: Reader, name: String, expr: Expression ) extends Statement
  case class Call( pos: Reader, name: String ) extends Statement
  case class Write( expr: Expression ) extends Statement
  case class Sequence( stats: List[Statement] ) extends Statement
  case class If( cond: Condition, stat: Statement ) extends Statement
  case class While( cond: Condition, stat: Statement ) extends Statement

  abstract class Condition
  case class Odd( expr: Expression ) extends Condition
  case class Comparison( left: Expression, comp: String, right: Expression ) extends Condition

  abstract class Expression
  case class Negate( x: Expression ) extends Expression
  case class Operation( left: Expression, op: String, right: Expression ) extends Expression
  case class Number( n: Int ) extends Expression
  case class Ident( pos: Reader, name: String ) extends Expression

  run(
    """
      |const max = 10;
      |var arg, ret;
      |
      |procedure isprime;
      |var i;
      |begin
      |  ret := 1;
      |  i := 2;
      |  while i < arg do
      |  begin
      |    if arg / i * i = arg then
      |    begin
      |      ret := 0;
      |      i := arg
      |    end;
      |    i := i + 1
      |  end
      |end;
      |
      |procedure primes;
      |begin
      |  arg := 2;
      |  while arg < max do
      |  begin
      |    call isprime;
      |    if ret = 1 then write arg;
      |    arg := arg + 1
      |  end
      |end;
      |
      |call primes.
    """.stripMargin
  )

}
```

### output

2
3
5
7


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
  <version>0.2.1</version>
</dependency>
```

Add the following to your `build.sbt` file to use Backslash in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "pattern-matcher" % "0.2.1"
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