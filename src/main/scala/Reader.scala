//@
package xyz.hyperreal.matcher


abstract class Reader {

  def eoi: Boolean

  def more = !eoi

  def ch: Char

  def next: Reader

  def line: Int

  def col: Int

  def before( that: Reader ) = line < that.line || (line == that.line && col < that.col)

  def substring( end: Reader ): String

  def lineText: String

  def errorText = lineText + '\n' + (" "*(col - 1)) + "^\n"

  def longErrorText = s"parser error on line $line, at column $col:\n" + errorText

  override def toString = s"line $line, col $col: $lineText"

}

class StringReader private ( s: String, val idx: Int, val line: Int, val col: Int ) extends Reader {

  def this( s: String ) = this( s, 0, 1, 1 )

  private def problem = sys.error( s"end of input: [$line, $col]" )

  override lazy val eoi: Boolean = idx == s.length

  override lazy val ch: Char =
    if (eoi)
      problem
    else
      s( idx )

  override lazy val next =
    if (eoi)
      problem
    else if (ch == '\n')
      new StringReader( s, idx + 1, line + 1, 1 )
    else
      new StringReader( s, idx + 1, line, col + 1 )

  def substring( end: Reader ) = s.substring( idx, end.asInstanceOf[StringReader].idx )

  override def lineText = {
    var ind = idx

    while (ind > 0 && s(ind - 1) != '\n' )
      ind -= 1

    s.indexOf( '\n', ind ) match {
      case -1 => s substring ind
      case end => s.substring( ind, end )
    }
  }

}
