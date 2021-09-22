package io.github.edadma.pattern_matcher

import io.github.edadma.char_reader.CharReader

object Example1 extends App with Matchers[CharReader] {

  delimiters ++= List("+", "-", "*", "/", "(", ")")

  def input: Example1.Matcher[Int] = matchall(expression)

  def additive: Matcher[(Int, Int) => Int] = ("+" | "-") ^^ {
    case "+" => _ + _
    case "-" => _ - _
  }

  def sign: Matcher[Int => Int] = opt("+" | "-") ^^ {
    case Some("-") => -_
    case _ => identity
  }

  def multiplicative: Matcher[(Int, Int) => Int] = ("*" | "/") ^^ {
    case "*" => _ * _
    case "/" => _ / _
  }

  def expression: Matcher[Int] = sign ~ term ~ rep(additive ~ term) ^^ {
    case s ~ n ~ l => (l foldLeft s(n)) { case (x, f ~ y) => f(x, y) }
  }

  def term: Example1.Matcher[Int] = factor ~ rep(multiplicative ~ factor) ^^ {
    case n ~ l => (l foldLeft n) { case (x, f ~ y) => f(x, y) }
  }

  def factor: Example1.Matcher[Int] = integerLit | "(" ~> expression <~ ")"

  def run(s: String): Unit =
    input(CharReader.fromString(s)) match {
      case Match(result, _) => println(result)
      case m: Mismatch => Console.err.println(m.errorString)
    }

  run("-3 + 4 * (-5)")
  run("5")
  run("2 +")

}
