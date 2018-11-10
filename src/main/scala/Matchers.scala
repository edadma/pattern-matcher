//@
package xyz.hyperreal.matcher

import scala.collection.mutable.{HashMap, ListBuffer}


/**
  * Provides methods for coding grammars.
  *
  * @tparam Input the type of [[Reader]] that will be used
  */
class Matchers[Input <: Reader] {

  private val groupmap = new HashMap[String, (Input, Input)]

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
    def map[S]( f: R => S ): MatcherResult[S]
  }

  /**
    * Represents a successful match.
    *
    * @param result result value of the match
    * @param next next character of input
    * @tparam R type of result value
    */
  case class Match[R]( result: R, next: Input ) extends MatcherResult[R] {
    def map[S]( f: R => S ) = Match( f(result), next )
  }

  /**
    * Represents an unsuccessful match.
    *
    * @param next character at which the mismatch occurred
    */
  case class Mismatch( next: Input ) extends MatcherResult {
    def map[S]( f: Nothing => S ) = this
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
    def map[S]( f: R => S ): Matcher[S] = this( _ ) map f

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is a tuple containing the result values of the two matchers.
      *
      * @param m the second matcher to be applied only if this matcher succeeds
      * @tparam S the type of the result value of the second matcher
      * @return the new sequential matcher
      */
    def ~ [S]( m: => Matcher[S] ): Matcher[R ~ S] = { in =>
      this( in ) match {
        case Match( a, in1 ) =>
          m( in1 ) match {
            case Match( b, in2 ) => Match( new ~(a, b), in2 )
            case f => f.asInstanceOf[Mismatch]
          }
        case f => f.asInstanceOf[Mismatch]
      }
    }

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is that of this matcher.
      *
      * @param m the second matcher
      * @tparam S the type of the result value of the second matcher
      * @return the new matcher
      */
    def <~ [S]( m: => Matcher[S] ) = (this ~ m) ^^ { case a ~ _ => a }

    /**
      * Returns a matcher that applies this matcher and another one sequentially and whose result value is that of the second matcher.
      *
      * @param m the second matcher
      * @tparam S the type of the result value of the second matcher
      * @return the new matcher
      */
    def ~> [S]( m: => Matcher[S] ) = (this ~ m) ^^ { case _ ~ b => b }

    /**
      * Returns a matcher that applies this matcher or alternatively another matcher if this matcher fails.
      *
      * @param m the alternate matcher
      * @tparam S the type of the result value of the alternate
      * @return the new matcher
      */
    def | [S >: R]( m: => Matcher[S] ): Matcher[S] = { in =>
      this( in ) match {
        case res: Match[R] => res
        case _ => m( in )
      }
    }

    def ^^ [S]( f: R => S ) = map( f )

    def ^^^ [S]( v0: => S ) = { lazy val v = v0
      map (_ => v)
    }

  }

  def clear = groupmap.clear

  def capture[S]( name: String, m: => Matcher[S] ): Matcher[S] = { in =>
    val start = in

    m( in ) match {
      case res@Match( _, next ) =>
        groupmap(name) = (start, next)
        res
      case res => res
    }
  }

  def matched[S]( m: => Matcher[S] ): Matcher[(Input, Input)] = { in =>
    val start = in

    m( in ) match {
      case Match( _, next ) => Match( (start, next), next )
      case res => res.asInstanceOf[Mismatch]
    }
  }

  def matchedString[S]( m: => Matcher[S] ) = matched(m) ^^ { case (s, e) => s substring e }

  def group( name: String ) = groupmap get name

  def substring( name: String ) =
    groupmap get name map { case (start, end) => start substring end }

  def rep1[S]( m: => Matcher[S] ) = m ~ rep(m) ^^ { case f ~ r => f :: r }

  def rep[S]( m0: => Matcher[S] ): Matcher[List[S]] = { in => lazy val m = m0
    val buf = new ListBuffer[S]
    val m1 = m

    def rep( in1: Input ): MatcherResult[List[S]] =
      m1( in1 ) match {
        case Match( v, r ) =>
          buf += v
          rep( r )
        case Mismatch( _ ) => Match( buf.toList, in1 )
      }

      rep( in )
  }

  def anyOf( cs: Char* ) = cls( cs contains _ )

  def noneOf( cs: Char* ) = cls( !cs.contains(_) )

  def opt[S]( m: => Matcher[S] ) = m ^^ (Some( _ )) | succeed( None )

  def not[S]( m: Matcher[S] ): Matcher[Unit] = { in =>
    m( in ) match {
      case Match( _, _ ) => Mismatch( in )
      case Mismatch( _ ) => Match( (), in )
    }
  }

  def guard[S]( m: => Matcher[S] ): Matcher[S] = { in =>
    m( in ) match {
      case Match( r, _ ) => Match( r, in )
      case f => f
    }
  }

  def soi: Matcher[Unit] =
    in =>
      if (in.soi)
        Match( (), in )
      else
        Mismatch( in )

  def eoi: Matcher[Unit] =
    in =>
      if (in.eoi)
        Match( (), in )
      else
        Mismatch( in )

  def succeed[R]( r: R ): Matcher[R] = Match( r, _ )

  def fail: Matcher[Nothing] = Mismatch( _ )

  def cls( pred: Char => Boolean ): Matcher[Char] = { in =>
    if (in.more && pred( in.ch ))
      Match( in.ch, in.next.asInstanceOf[Input] )
    else
      Mismatch( in )
  }

  implicit def ch( c: Char ): Matcher[Char] = cls( _ == c )

  implicit def str( s: String ): Matcher[String] = { in =>
    def str( idx: Int, in1: Input ): MatcherResult[String] =
      if (idx < s.length)
        if (in1.more && s.charAt( idx ) == in1.ch)
          str( idx + 1, in1.next.asInstanceOf[Input] )
        else
          Mismatch( in1 )
      else
        Match( s, in1 )

    str( 0, in )
  }

  case class ~[+A, +B]( a: A, b: B )

  private val HEXDIGITSET = ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9') toSet

  def hexdigit: Matcher[Char] = cls( HEXDIGITSET )

  def letterOrDigit: Matcher[Char] = cls( _.isLetterOrDigit )

  def letter: Matcher[Char] = cls( _.isLetter )

  def lower: Matcher[Char] = cls( _.isLower )

  def upper: Matcher[Char] = cls( _.isUpper )

  def digit: Matcher[Char] = cls( _.isDigit )

  def space: Matcher[Char] = cls( _.isSpaceChar )

  def lookbehind( pred: Char => Boolean ): Matcher[Char] = { in =>
    if (!in.soi && pred( in.lookbehind ))
      Match( in.lookbehind, in )
    else
      Mismatch( in )
  }

//  def boundary( start: Boolean, predb: Char => Boolean,
//                end: Boolean, predf: Char => Boolean ) =
//    (lookbehind( predb ) | (if (start) soi else fail)) ~ guard(cls(predf) | (if (end) eoi else fail))

}
