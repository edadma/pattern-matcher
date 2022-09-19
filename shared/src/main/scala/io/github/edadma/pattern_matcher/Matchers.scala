package io.github.edadma.pattern_matcher

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Provides methods for coding character pattern matchers.
  */
trait Matchers[Input <: CharReader] {

  private val groupmap = new mutable.HashMap[String, (Input, Input)]

  /**
    * Abstract class for [[Matcher]] results
    *
    * @tparam R type of result value
    */
  abstract class MatcherResult[+R] {

    /**
      * Next character of input.
      */
    val next: Input

    /**
      * Applies of function to matcher result value.
      *
      * @param f funciton to be applied to result value
      * @tparam S type of new result value
      * @return new matcher result
      */
    def map[S](f: R => S): MatcherResult[S]
  }

  /**
    * Represents a successful match.
    *
    * @param result result value of the match
    * @param next   next character of input
    * @tparam R type of result value
    */
  case class Match[R](result: R, next: Input) extends MatcherResult[R] {
    def map[S](f: R => S): MatcherResult[S] = Match(f(result), next)
  }

  /**
    * Represents an unsuccessful match.
    *
    * @param next character at which the mismatch occurred
    */
  case class Mismatch(msg: String, next: Input) extends MatcherResult[Nothing] {
    def map[S](f: Nothing => S): MatcherResult[S] = this

    def errorString: String = next.longErrorText(msg)

    def error: Nothing = sys.error(errorString)
  }

  /**
    * Abstract matcher.  A matcher is a function that maps character input to a result value.
    *
    * @tparam R type of result value
    */
  abstract class Matcher[+R] extends (Input => MatcherResult[R]) {

    /**
      * Returns a new matcher whose result value is transformed by a function.
      *
      * @param f function applied to the result value of this matcher
      * @tparam S type of the result value of the matcher being returned
      * @return a new matcher with a mapped result value
      */
    def map[S](f: R => S): Matcher[S] = this (_) map f

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is a tuple containing the result values of the two matchers.
      *
      * @param m the second matcher to be applied only if this matcher succeeds
      * @tparam S the type of the result value of the second matcher
      * @return the new sequential matcher
      */
    def ~[S](m: => Matcher[S]): Matcher[R ~ S] = {
      lazy val m1 = m

      { in =>
        this (in) match {
          case Match(a, in1) =>
            m1(in1) match {
              case Match(b, in2) => Match(new ~(a, b), in2)
              case f => f.asInstanceOf[Mismatch]
            }
          case f => f.asInstanceOf[Mismatch]
        }
      }
    }

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is that of this matcher.
      *
      * @param m the second matcher
      * @tparam S the type of the result value of the second matcher
      * @return the new matcher
      */
    def <~[S](m: => Matcher[S]): Matcher[R] = (this ~ m) ^^ { case a ~ _ => a }

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is that of the second matcher.
      *
      * @param m the second matcher
      * @tparam S the type of the result value of the second matcher
      * @return the new matcher
      */
    def ~>[S](m: => Matcher[S]): Matcher[S] = (this ~ m) ^^ { case _ ~ b => b }

    /**
      * Returns a matcher that applies this matcher or alternatively another matcher if this matcher fails.
      *
      * @param m the alternate matcher
      * @tparam S the type of the result value of the alternate
      * @return the new matcher
      */
    def |[S >: R](m: => Matcher[S]): Matcher[S] = {
      lazy val m1 = m

      { in =>
        this (in) match {
          case res: Match[_] => res
          case _ => m1(in)
        }
      }
    }

    /**
      * Returns a matcher whose result value is transformed by a function.
      *
      * @param f the function applied to the result value
      * @tparam S the type of the transformed result value
      */
    def ^^[S](f: R => S): Matcher[S] = map(f)

    def ^^^[S](v: => S): Matcher[S] = map(_ => v)

    def * : Matcher[List[R]] = rep(this)

    def + : Matcher[List[R]] = rep1(this)

    def ? : Matcher[Option[R]] = opt(this)

    def withMessage(msg: String): Matcher[R] = { in =>
      this (in) match {
        case Mismatch(_, next) => Mismatch(msg, next)
        case other => other
      }
    }

  }

  /**
    * Clears capture groups.
    */
  def clear(): Unit = groupmap.clear()

  def capture[S](name: String, m: => Matcher[S]): Matcher[S] = {
    lazy val m1 = m

    { in =>
      m1(in) match {
        case res@Match(_, next) =>
          groupmap(name) = (in, next)
          res
        case res => res
      }
    }
  }

  def matched[S](m: => Matcher[S]): Matcher[(Input, Input)] = {
    lazy val m1 = m

    { in =>
      m1(in) match {
        case Match(_, next) => Match((in, next), next)
        case res => res.asInstanceOf[Mismatch]
      }
    }
  }

  def string[S](m: => Matcher[S]): Matcher[String] = matched(m) ^^ {
    case (s, e) => s substring e
  }

  def tap[S](name: String)(m: => Matcher[S]): Matcher[S] = {
    lazy val m1 = m

    { in =>
      println(s"$in $name >>>")

      val result = m1(in)

      result match {
        case Match(result, next) =>
          println(s"$in $name <<< match $result $next")
        case Mismatch(msg, next) => println(s"$in $name <<< fail")
      }

      result
    }
  }

  /**
    * Returns capture group.
    *
    * @param name the name of the capture group to return
    * @return a capture group with is a pair of input objects: the first is the first character in the group, the second is the next input character after the end of the group.
    */
  def group(name: String): Option[(Input, Input)] = groupmap get name

  /**
    * Returns the substring from a capture group.
    *
    * @param name the name of the capture group
    */
  def substring(name: String): Option[String] =
    groupmap get name map { case (start, end) => start substring end }

  def rep1[S](m: => Matcher[S]): Matcher[List[S]] = {
    lazy val m1 = m

    m1 ~ rep(m1) ^^ { case f ~ r => f :: r }
  }

  def rep[S](m: => Matcher[S]): Matcher[List[S]] = {
    lazy val m1 = m

    { in =>
      val buf = new ListBuffer[S]

      @scala.annotation.tailrec
      def rep(in1: Input): MatcherResult[List[S]] =
        m1(in1) match {
          case Match(v, r) =>
            buf += v
            rep(r)
          case Mismatch(_, _) => Match(buf.toList, in1)
        }

      rep(in)
    }
  }

  def repu[S](m: => Matcher[S]): Matcher[Unit] = {
    lazy val m1 = m

    { in =>
      @scala.annotation.tailrec
      def repu(in1: Input): MatcherResult[Unit] =
        m1(in1) match {
          case Match(_, r) => repu(r)
          case Mismatch(_, _) => Match((), in1)
        }

      repu(in)
    }
  }

  def rep1sep[T, U](m: => Matcher[T], sep: => Matcher[U]): Matcher[List[T]] = {
    lazy val m1 = m
    lazy val s1 = sep

    m1 ~ rep(s1 ~> m1) ^^ { case r ~ rs => r :: rs } | succeed(Nil)
  }

  def repsep[T, U](m: => Matcher[T], sep: => Matcher[U]): Matcher[List[T]] =
    opt(rep1sep(m, sep)) ^^ {
      case None => Nil
      case Some(l) => l
    }

  /**
    * Returns a matcher that will match any of a list of characters.
    *
    * @param cs argument list of characters
    * @return a matcher that only matches a character from a list
    */
  def anyOf(cs: Char*): Matcher[Char] =
    elem(cs contains _,
      "expected one of: " + cs.map(c => s"'$c'").mkString(", "))

  /**
    * Returns a matcher that will matcher any character not in a list of characters.
    *
    * @param cs argument list of characters
    * @return a matcher that only matches a character not on a list
    */
  def noneOf(cs: Char*): Matcher[Char] =
    notElem(cs contains _,
      "not expecting any of: " + cs.map(c => s"'$c'").mkString(", "))

  /**
    * Returns a matcher that allows a matcher to succeed optionally.
    *
    * @param m the matcher to apply to the input.
    * @tparam S the type of the optional result value
    * @return a matcher with an optional result value
    */
  def opt[S](m: => Matcher[S]): Matcher[Option[S]] = {
    m ^^ (Some(_)) | succeed(None)
  }

  /**
    * Returns a matcher that negates the result of the given matcher. No input is consumed.
    *
    * @param m the matcher whose result is negated
    * @tparam S the type of the result value of the given matcher
    * @return the new matcher
    */
  def not[S](m: => Matcher[S]): Matcher[Unit] = {
    lazy val m1 = m

    { in =>
      m1(in) match {
        case Match(_, _) => Mismatch("negation", in)
        case Mismatch(_, _) => Match((), in)
      }
    }
  }

  /**
    * Returns a matcher whose result is the same as the given matcher, but without consuming any input.
    *
    * @param m the given matcher
    * @tparam S the type of the result value
    * @return the new matcher
    */
  def guard[S](m: => Matcher[S]): Matcher[S] = {
    lazy val m1 = m

    { in =>
      m1(in) match {
        case Match(r, _) => Match(r, in)
        case f => f
      }
    }
  }

  def pos: Matcher[Input] = in => Match(in, in)

  def affect(affect: CharReader => Unit): Matcher[Unit] = { in =>
    affect(in)
    Match((), in)
  }

  /**
    * Returns a zero-length matcher that succeeds at the start of input.
    *
    * @return a matcher that succeeds at the start of input, fails otherwise.
    */
  def soi: Matcher[Unit] =
    in =>
      if (in.soi)
        Match((), in)
      else
        Mismatch("expected start of input", in)

  /**
    * Returns a zero-length matcher that succeeds at the end of input.
    *
    * @return a matcher that succeeds at the end of input, fails otherwise.
    */
  def eoi: Matcher[Unit] =
    in =>
      if (in.eoi)
        Match((), in)
      else
        Mismatch("expected end of input", in)

  /**
    * Returns a matcher that always succeeds.
    *
    * @param r the result value
    * @tparam R the type of result value
    * @return a matcher that always succeeds with a result value
    */
  def succeed[R](r: => R): Matcher[R] = Match(r, _)

  /**
    * Returns a matcher that always fails.
    *
    * @return a matcher that always fails with a result containing the current point in the input stream
    */
  def fail(msg: String): Matcher[Nothing] = Mismatch(msg, _)

  /**
    * Returns a matcher that matches the current input character if it is a member of a class of characters.
    *
    * @param pred predicate that determines if the current input character matches
    * @return a matcher for matching character classes
    */
  def elem(pred: Char => Boolean,
           msg: String = "not in character class"): Matcher[Char] = { in =>
    if (in.more && pred(in.ch))
      Match(in.ch, in.next.asInstanceOf[Input])
    else
      Mismatch(msg, in)
  }

  def notElem(pred: Char => Boolean,
              msg: String = "in character class"): Matcher[Char] =
    elem(!pred(_))

  /**
    * Returns a matcher that always succeeds as long as there is input remaining, matching one character.
    *
    * @return a matcher with the next input character as its result value, failing if there is no more input
    */
  def char: Matcher[Char] = elem(_ => true)

  /**
    * Returns a matcher for a specific character. This combinator is an implicit function so that character literals can be lifted to the corresponding character matcher.
    *
    * For example
    *
    * {{{
    *   def tag: Matcher[String] = '<' ~> (letter|'_') ~ rep(letter|digit|'-'|'_'|'.') <~ '>'
    * }}}
    *
    * @param c the character to be matched
    * @return the character matcher
    */
  implicit def ch(c: Char): Matcher[Char] = elem(_ == c, s"expected '$c'")

  /**
    * Returns a matcher to match against a string.
    *
    * For example
    *
    * {{{
    *   def bracketed: Matcher[List[Char]] = "[[" ~> rep(not("]]") ~> char) <~ "]]"
    * }}}
    *
    * @param s the string to match
    * @return a matcher that matches against a string, with that string as its result value if it succeeds
    */
  def str(s: String): Matcher[String] = { in =>
    @scala.annotation.tailrec
    def str(idx: Int, in1: Input): MatcherResult[String] =
      if (idx < s.length)
        if (in1.more && s.charAt(idx) == in1.ch)
          str(idx + 1, in1.next.asInstanceOf[Input])
        else
          Mismatch(s"expected '$s'", in)
      else
        Match(s, in1)

    str(0, in)
  }

  /**
    * Case class for return sequence matcher results that can be pattern matched.
    *
    * @param a left result value
    * @param b right result value
    * @tparam A type of left result value
    * @tparam B type of right result value
    */
  case class ~[+A, +B](a: A, b: B)

  private val HEXDIGITSET = ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9') toSet

  /** The set of reserved identifiers */
  val reserved = new mutable.HashSet[String]

  /** The set of delimiters */
  val delimiters = new mutable.HashSet[String]

  /** Returns a hex digit character matcher. */
  def hexdigit: Matcher[Char] = elem(HEXDIGITSET, "expected hexadecimal digit")

  /** Returns a letter or digit character matcher. */
  def letterOrDigit: Matcher[Char] =
    elem(_.isLetterOrDigit, "expected a letter or digit")

  /** Returns a letter character matcher. */
  def letter: Matcher[Char] = elem(_.isLetter, "expected a letter")

  /** Returns a lower case character matcher. */
  def lower: Matcher[Char] = elem(_.isLower, "expected a lower case letter")

  /** Returns an upper case character matcher. */
  def upper: Matcher[Char] = elem(_.isUpper, "expected an upper case letter")

  /** Returns a digit character matcher. */
  def digit: Matcher[Char] = elem(_.isDigit, "expected a digit")

  /** Returns a space character matcher. */
  def space: Matcher[Char] = elem(_.isWhitespace, "expected a space character")

  def identChar(c: Char): Boolean = c.isLetter | c == '_'

  def identOrReserved: Matcher[String] =
    t(string(elem(identChar) ~ rep(elem(identChar) | digit)))

  def ident: Matcher[String] = { in =>
    identOrReserved(in) match {
      case res@Match(m, _) =>
        if (reserved contains m)
          Mismatch(s"reserved: '$m'", in)
        else
          res
      case m => m.asInstanceOf[Mismatch]
    }
  }

  private lazy val _delim: Matcher[String] =
    (delimiters.toList sortWith (_ > _) map str)
      .foldRight(fail("expected a delimiter"): Matcher[String])(_ | _)

  def delimiter: Matcher[String] = t(_delim)

  implicit def keyword(s: String): Matcher[String] = { (in: Input) =>
    if (!reserved.contains(s) && !delimiters.contains(s))
      in.error(s"not reserved: $s")
    else
      (if (identChar(s.head)) identOrReserved else delimiter)
        .withMessage(s"expected '$s'")(in) match {
        case res@Match(m, _) =>
          if (m == s)
            res
          else
            Mismatch(s"expected '$s'", in)
        case m => m.asInstanceOf[Mismatch]
      }
  }

  def lineComment: Matcher[_] = '/' ~ '/'

  def blockCommentStart: Matcher[_] = '/' ~ '*'

  def blockCommentEnd: Matcher[_] = '*' ~ '/'

  def whitespace: Matcher[Unit] =
    repu(
      space |
        lineComment ~ repu(noneOf('\n')) |
        blockCommentStart ~ repu(not(blockCommentEnd) ~> char) <~ blockCommentEnd |
        blockCommentStart ~ fail("unclosed comment")
    )

  def t[S](m: => Matcher[S]): Matcher[S] = whitespace ~> m <~ whitespace

  def matchall[R](m: Matcher[R]): Matcher[R] = m <~ eoi

  def singleStringLit: Matcher[String] =
    t('\'' ~> string(rep(noneOf('\'', '\n', CharReader.EOI))) <~ '\'')

  def doubleStringLit: Matcher[String] =
    t('"' ~> string(rep(noneOf('"', '\n', CharReader.EOI))) <~ '"')

  def backStringLit: Matcher[String] =
    t('`' ~> string(rep(noneOf('`', '\n', CharReader.EOI))) <~ '`')

  def digits: Matcher[String] = rep1(digit) ^^ (_ mkString)

  def integerLit: Matcher[Int] = t(digits) ^^ (_.toInt)

  def floatLit: Matcher[Double] =
    t(
      string(
        digits ~ '.' ~ digits ~ optExponent |
          '.' ~ digits ~ optExponent |
          digits ~ exponent)) ^^ (_.toDouble)

  private def exponent = (ch('e') | 'E') ~ opt(ch('+') | '-') ~ digits ^^ {
    case e ~ None ~ exp => List(e, exp) mkString
    case e ~ Some(s) ~ exp => List(e, s, exp) mkString
  }

  private def optExponent = opt(exponent) ^^ {
    case None => ""
    case Some(e) => e
  }

  /**
    * Returns a zero-length matcher that succeeds if the previous input character is a member of a character class.
    *
    * @param pred predicate that determines inclusion in a character class
    */
  def lookbehind(pred: Char => Boolean,
                 msg: String = "not in character class"): Matcher[Char] = {
    in =>
      if (!in.soi && pred(in.prev.get))
        Match(in.prev.get, in)
      else
        Mismatch(msg, in)
  }
}
