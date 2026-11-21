package galileo.plot

// Minimal JSON writer for plot specifications (no dependencies)
object JsonOut {
  def str( s: String ): String = {
    val sb = new StringBuilder( "\"" )
    s.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c => sb += c
    }
    sb += '"'
    sb.toString
  }

  // NaN and infinities become null, which Plotly treats as a gap
  def num( d: Double ): String =
    if ( d.isNaN || d.isInfinite ) "null"
    else if ( d == math.rint( d ) && math.abs( d ) < 1e15 ) d.toLong.toString
    else java.lang.Double.toString( d )

  def arr( xs: Iterable[ String ] ): String = xs.mkString( "[", ",", "]" )
  def nums( xs: Iterable[ Double ] ): String = arr( xs.map( num ) )
  def obj( fields: ( String, String )* ): String = fields.map { case ( k, v ) => str( k ) + ":" + v }.mkString( "{", ",", "}" )
}
