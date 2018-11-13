//@
package xyz.hyperreal.matcher


abstract class Reader {

  def tabs: Int

  def soi: Boolean

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

  protected def error( msg: String ) = sys.error( s"$msg: [$line, $col]" )

  protected def eoiError = error( "end of input" )

  def lineText = {
    val buf = new StringBuilder
    var zcol = 0

    lineString foreach {
      case '\t' =>
        val len = tabs - zcol%tabs

        buf ++= " "*len
        zcol += len
      case '\n' => error( "found newline in string from lineString()" )
      case c =>
        buf += c
        zcol += 1
    }

    buf.toString
  }

  def errorText = lineText + '\n' + (" "*(col - 1)) + "^\n"

  def longErrorText = s"matcher error on line $line, at column $col:\n" + errorText

  override def toString = s"line $line, col $col: $lineString"

}

class StringReader private ( s: String, val idx: Int, val line: Int, val col: Int, val tabs: Int ) extends Reader {

  def this( s: String, tabs: Int = 4 ) = this( s, 0, 1, 1, tabs )

  override lazy val soi: Boolean = idx == 0

  override lazy val eoi: Boolean = idx == s.length

  override lazy val ch: Char =
    if (eoi)
      eoiError
    else
      s.charAt( idx )

  override lazy val next =
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

class SourceReader( ) extends Reader {
  override def tabs: Int = ???
  override def soi: Boolean = ???
  override def eoi: Boolean = ???
  override def ch: Char = ???
  override def next: Reader = ???
  override def prev: Char = ???
  override def line: Int = ???
  override def col: Int = ???
  override def substring(end:  Reader): String = ???
  override def lineString: String = ???
}