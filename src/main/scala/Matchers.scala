//@
package xyz.hyperreal.matcher

import scala.collection.mutable.{HashMap, ListBuffer}


class Matchers[Input <: Reader] {

  private val groupmap = new HashMap[String, (Input, Input)]

  abstract class MatcherResult[+R] {
    val next: Input

    def map[S]( f: R => S ): MatcherResult[S]
  }

  case class Match[R]( result: R, next: Input ) extends MatcherResult[R] {
    def map[S]( f: R => S ) = Match( f(result), next )
  }

  case class Mismatch( next: Input ) extends MatcherResult {
    def map[S]( f: Nothing => S ) = this
  }

  abstract class Matcher[+R] extends (Input => MatcherResult[R]) {

    def map[S]( f: R => S ): Matcher[S] = this( _ ) map f

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

    def <~ [S]( m: => Matcher[S] ) = (this ~ m) ^^ { case a ~ _ => a }

    def ~> [S]( m: => Matcher[S] ) = (this ~ m) ^^ { case _ ~ b => b }

    def | [S >: R]( q: => Matcher[S] ): Matcher[S] = { in =>
      this( in ) match {
        case res: Match[R] => res
        case _ => q( in )
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

}
