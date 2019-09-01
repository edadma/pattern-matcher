//@
package xyz.hyperreal.pattern_matcher

import java.io.{FileInputStream, InputStream}


object Reader {

  def fromString( s: String ) = new StringReader( s )

  def fromInputStream( s: InputStream, enc: String ): IteratorReader = fromInputStream( s )( io.Codec(enc) )

  def fromInputStream( s: InputStream )( implicit codec: io.Codec ) = fromSource( io.Source.fromInputStream(s)(codec) )

  def fromSource( s: io.Source ) = new IteratorReader( s )

  def fromFile( file: String )( implicit codec: io.Codec ) = fromInputStream( new FileInputStream(file) )

  val EOI = '\u001A'

}

abstract class Reader {

  def tabs: Int

  lazy val soi = line == 1 && col == 1

  def eoi: Boolean

  def more = !eoi

  def ch: Char

  def next: Reader

  def prev: Char

  def line: Int

  def col: Int

  def before( that: Reader ) = line < that.line || (line == that.line && col < that.col)

  def substring( end: Reader ): String

  def lineString: String

  def error( msg: String ) = sys.error( longErrorText(msg) )

  def eoiError = error( "end of input" )

  def lineText = {
    val buf = new StringBuilder
    var zcol = 0

    lineString foreach {
      case '\t' =>
        val len = tabs - zcol%tabs

        buf ++= " "*len
        zcol += len
      case '\n' => sys.error( "found newline in string from lineString()" )
      case c =>
        buf += c
        zcol += 1
    }

    buf.toString
  }

  def errorText = lineText + '\n' + (" "*(col - 1)) + "^\n"

  def longErrorText( msg: String ) = s"$msg (line $line, column $col):\n" + errorText

  override def toString = s"<$line, $col>"

}

class StringReader private ( s: String, val idx: Int, val line: Int, val col: Int, val tabs: Int ) extends Reader {

  def this( s: String, tabs: Int = 4 ) = this( s, 0, 1, 1, tabs )

  override lazy val eoi: Boolean = idx == s.length

  override lazy val ch: Char =
    if (eoi)
      Reader.EOI
    else
      s.charAt( idx )

  lazy val next =
    if (eoi)
      eoiError
    else if (ch == '\t')
      new StringReader( s, idx + 1, line, col + (tabs - (col - 1)%tabs), tabs )
    else if (ch == '\n')
      new StringReader( s, idx + 1, line + 1, 1, tabs )
    else
      new StringReader( s, idx + 1, line, col + 1, tabs )

  lazy val prev =
    if (soi)
      error( "no previous character" )
    else
      s.charAt( idx - 1 )

  def substring( end: Reader ) = s.substring( idx, end.asInstanceOf[StringReader].idx )

  override def lineString = {
    var ind = idx

    while (ind > 0 && s(ind - 1) != '\n' )
      ind -= 1

    s.indexOf( '\n', ind ) match {
      case -1 => s substring ind
      case end => s.substring( ind, end )
    }
  }

}

class IteratorReader private ( it: Iterator[Char], val line: Int, val col: Int, val tabs: Int, val _prev: Char,
                               _start: IteratorReader ) extends Reader {

  def this( it: Iterator[Char], tabs: Int = 4 ) = this( it, 1, 1, tabs, 0, null )

  private val start = if (_start eq null) this else _start
  private var cur: Char = _

  lazy val eoi = !more

  override lazy val more =
    if (it.hasNext) {
      cur = it.next
      true
    } else
      false

  lazy val ch =
    if (eoi)
      Reader.EOI
    else
      cur

  lazy val next =
    if (eoi)
      eoiError
    else if (ch == '\t')
      new IteratorReader( it, line, col + (tabs - (col - 1)%tabs), tabs, ch, start )
    else if (ch == '\n')
      new IteratorReader( it, line + 1, 1, tabs, ch, null )
    else
      new IteratorReader( it, line, col + 1, tabs, ch, start )

  lazy val prev =
    if (soi)
      error( "no previous character" )
    else
      _prev

  override def substring( end:  Reader ) = {
    val buf = new StringBuilder
    var x: Reader = this

    while (x ne end) {
      buf += x.ch
      x = x.next
    }

    buf.toString
  }

  override def lineString = {
    val buf = new StringBuilder
    var x: Reader = this.start

    while (x.more && x.ch != '\n') {
      buf += x.ch
      x = x.next
    }

    buf.toString
  }

}

class CharReader( val tabs: Int, val ch: Char, val next: Reader, val prev: Char,
                  val line: Int, val col: Int ) extends Reader {

  val eoi = false

  def substring( end:  Reader ) = s"$ch${next.substring( end )}"

  def lineString: String = ???

}

class IndentationReader( r: Reader ) extends Reader {

  override def tabs: Int = ???
  override def eoi: Boolean = ???
  override def ch: Char = ???
  override def next: Reader = ???
  override def prev: Char = ???
  override def line: Int = ???
  override def col: Int = ???
  override def substring( end:  Reader ): String = ???
  override def lineString: String = ???

}